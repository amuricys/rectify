{-# LANGUAGE AllowAmbiguousTypes #-}

module App.WsApp where

import SimulatedAnnealing.Surface.Config
import Control.Concurrent.STM (TVar, atomically, modifyTVar, newTVarIO, readTVarIO, writeTVar)
import Control.Exception (finally)
import Control.Monad (forever, when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson (FromJSON, ToJSON, encode)
import Effectful
import Effectful.Reader.Dynamic (Reader, runReader)
import GHC.Generics (Generic)
import Network.Wai (Application)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Handler.Warp qualified as Warp
import Network.Wai.Handler.WebSockets (websocketsOr)
import Network.Wai.Middleware.Cors
  ( CorsResourcePolicy (corsMethods, corsOrigins, corsRequestHeaders),
    cors,
    simpleCorsResourcePolicy,
  )
import Network.WebSockets
  ( Connection,
    DataMessage (..),
    PendingConnection,
    acceptRequest,
    defaultConnectionOptions,
    forkPingThread,
    receiveDataMessage,
    sendDataMessage,
    sendTextData,
    withPingThread,
  )
import Random
import Servant
  ( Application,
    Context (EmptyContext, (:.)),
    Handler,
    JSON,
    Post,
    Proxy (..),
    ReqBody,
    Server,
    serve,
    serveWithContext,
    (:<|>),
    type (:<|>) (..),
  )
import Servant.API.WebSocket (WebSocketPending)
import SimulatedAnnealing (Algorithm (..), Problem (..), SimState (..), problemToInitialSimState, step)
import SimulatedAnnealing.Surface.Problem (seed, surfaceProblem)
import SimulatedAnnealing.TSP.Problem (allCities, tspProblem)
import UnliftIO.Concurrent (forkIO, yield)
import Prelude

data RunState = Paused | Running | Stepping | ChangingAlgorithm
  deriving (Eq, Show, Generic)

data State = State {currentAlgorithm :: Algorithm, runState :: RunState}
  deriving (Eq, Show, Generic)

data Ctx = Ctx
  { stateVar :: TVar State,
    conn :: Connection
  }

effAction ::
  forall metric solution config beta es.
  ToJSON metric =>
  ToJSON solution =>
  ToJSON beta =>
  Problem
    (Eff '[Reader config, RandomEff, IOE])
    metric
    beta
    solution ->
  config ->
  Algorithm ->
  Ctx ->
  IO ()
effAction prob config algo ctx =
  runEff . runRandomPure seed . runReader config $ serverSendMessage @config algo prob ctx

wsApp :: MonadIO m => PendingConnection -> m ()
wsApp pending = liftIO $ do
  conn <- acceptRequest pending
  putStrLn "Client connected"
  -- Create a shared state variable where the communication between threads happens
  stateVar <- newTVarIO State {currentAlgorithm = Surface, runState = Paused}
  let ctx = Ctx {stateVar, conn}
      spConfig =
        Config
          { changeRange = 40,
            addThresh = 10.0,
            removeThresh = 0.5,
            thickness = 15.0,
            radius = 200.0
          }
  -- Send the initial state to the client
  -- Handle each problem in a separate thread
  _ <- forkIO (effAction (surfaceProblem @100 @100 @7 spConfig.radius spConfig.thickness) spConfig Surface ctx)
  _ <- forkIO (effAction (tspProblem @30 100 allCities) ("jeba" :: String) TSP ctx)
  _ <- forkIO (effAction (surfaceProblem @100 @100 @7 spConfig.radius 5) spConfig Reservoir ctx)
  -- Handle client messages in a new thread
  liftIO $ finally (handleClientMessages ctx) (putStrLn "Client disconnected")

      

-- TODO: Associate Algorithm type with Problem somehow, so that it becomes
-- impossible to call this function with a TSP with the wrong Problem record.
serverSendMessage :: forall config es metric solution beta .
  ( RandomEff :> es,
    Reader config :> es,
    ToJSON metric,
    ToJSON solution,
    ToJSON beta,
    IOE :> es
  ) =>
  Algorithm ->
  Problem (Eff es) metric beta solution ->
  Ctx ->
  Eff es ()
serverSendMessage thisThreadsAlgorithm problem Ctx {stateVar, conn} =
  do
    simState <- problemToInitialSimState problem
    -- When initializing the loop, have only the Surface thread send the initial state to the client
    when (thisThreadsAlgorithm == Surface) $
      liftIO $
        sendTextData conn (encode simState)
    loop simState
  where
    loop s = do
      -- Try reading from the shared state variable - if this thread's algorithm is not the
      -- one in the state variable, or the state is Paused, force another read to run by yielding
      -- If it's Running or Stepping, perform one step and send its result to the client.

      -- TODO: I don't like all this yielding. Feels like the thread should sleep instead
      -- and be woken up when it's supposed to do something.
      state <- liftIO $ readTVarIO stateVar
      case (state.currentAlgorithm, state.runState) of
        (a, Running) | a == thisThreadsAlgorithm -> do
          s' <- step problem s
          liftIO (sendTextData conn (encode s'))
          loop s'
        (a, Stepping) | a == thisThreadsAlgorithm -> do
          s' <- step problem s
          liftIO $ sendTextData conn (encode s')
          liftIO $ atomically $ modifyTVar stateVar (\st -> st {runState = Paused})
          loop s'
        (a, ChangingAlgorithm) | a == thisThreadsAlgorithm -> do
          liftIO $ sendTextData conn (encode s)
          liftIO $ atomically $ modifyTVar stateVar (\st -> st {runState = Paused})
          loop s
        _ -> do
          yield
          loop s

handleClientMessages :: Ctx -> IO ()
handleClientMessages Ctx {stateVar, conn} = do
  -- Ping the client every 30 seconds to keep the connection alive
  liftIO $ withPingThread conn 30 (putStrLn "Pinged!") $ forever $ do
    msg <- receiveDataMessage conn
    putStrLn $ "Received message from client: " ++ show msg
    state <- readTVarIO stateVar
    case (msg, state) of
      (Text "Unpause" _, State _ Paused) -> do
        putStrLn "Unpausing"
        atomically $ modifyTVar stateVar (\s -> s {runState = Running})
      (Text "Step" _, State _ Paused) -> do
        putStrLn "Stepping"
        atomically $ modifyTVar stateVar (\s -> s {runState = Stepping})
      (Text "Pause" _, State _ Running) -> do
        putStrLn "Pausing"
        atomically $ modifyTVar stateVar (\s -> s {runState = Paused})
      (Text "Surface" _, State _ _) -> do
        putStrLn "Starting Surface"
        atomically $ writeTVar stateVar State {runState = ChangingAlgorithm, currentAlgorithm = Surface}
      (Text "TSP" _, State _ _) -> do
        putStrLn "Starting TSP"
        atomically $ writeTVar stateVar State {runState = ChangingAlgorithm, currentAlgorithm = TSP}
      (Text "Reservoir" _, State _ _) -> do
        putStrLn "Starting Reservoir"
        atomically $ writeTVar stateVar State {runState = ChangingAlgorithm, currentAlgorithm = Reservoir}
      _ -> pure ()
