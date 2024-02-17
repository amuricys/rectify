module App where

import Control.Concurrent.STM (TVar, atomically, modifyTVar, newTVarIO, readTVarIO, writeTVar)
import Control.Exception (finally)
import Control.Monad (forever)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson (FromJSON, ToJSON, encode)
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
import Network.WebSockets (Connection, DataMessage (..), PendingConnection, acceptRequest, defaultConnectionOptions, forkPingThread, receiveDataMessage, sendDataMessage, sendTextData, withPingThread)
import Problem (seed, surfaceProblem)
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
    type (:>),
  )
import Servant.API.WebSocket (WebSocketPending)
import SimulatedAnnealing (Algorithm (..), Problem (..), SimState (..), problemToInitialSimState, step)
import UnliftIO.Concurrent (forkIO, yield)
import Prelude

-- Define a type for your API
type ServantType =
  "mypostendpoint" :> ReqBody '[JSON] MyRequest :> Post '[JSON] MyResponse
    :<|> "ws" :> WebSocketPending

data MyRequest = MyRequest {message :: String}
  deriving (Eq, Show, Generic)

instance FromJSON MyRequest

instance ToJSON MyRequest

data MyResponse = MyResponse {reply :: String}
  deriving (Eq, Show, Generic)

instance FromJSON MyResponse

instance ToJSON MyResponse

data RunState = Paused | Running | Stepping
  deriving (Eq, Show, Generic)

data State = State {currentAlgorithm :: Algorithm, runState :: RunState}
  deriving (Eq, Show, Generic)

data Ctx = Ctx
  { stateVar :: TVar State,
    conn :: Connection
  }

postHandler :: MyRequest -> Handler MyResponse
postHandler req = return $ MyResponse ("You sent: " ++ message req)

server :: Server ServantType
server = postHandler :<|> wsApp

app :: Application
app = serve (Proxy :: Proxy ServantType) server
  where
    ctx = EmptyContext

wsApp :: MonadIO m => PendingConnection -> m ()
wsApp pending = liftIO $ do
  conn <- acceptRequest pending
  putStrLn "Client connected"
  -- Create a shared state variable where the communication between threads happens
  stateVar <- newTVarIO State {currentAlgorithm = Surface, runState = Paused}
  let ctx = Ctx {stateVar, conn}
  handleAndSend ctx
  where
    handleAndSend ctx = do
      -- Handle client messages in a new thread
      _ <- forkIO $ finally (handleClientMessages ctx) (putStrLn "Client disconnected")
      -- Handle each problem in a separate thread
      _ <- forkIO $ serverSendMessage Surface (Problem.surfaceProblem @250 @250 @7 200 5) ctx
      _ <- forkIO $ serverSendMessage TSP (Problem.surfaceProblem @250 @250 @7 200 5) ctx
      _ <- forkIO $ serverSendMessage Reservoir (Problem.surfaceProblem @250 @250 @7 200 5) ctx
      -- The main thread then does nothing
      yield

-- TODO: Associate Algorithm type with Problem somehow, so that it becomes
-- impossible to call this function with a TSP with the wrong Problem record.
serverSendMessage ::
  Show metric =>
  ToJSON metric =>
  ToJSON solution =>
  Algorithm ->
  Problem metric beta solution ->
  Ctx ->
  IO ()
serverSendMessage thisThreadsAlgorithm problem Ctx {stateVar, conn} =
  loop stateVar conn problem (problemToInitialSimState problem seed)
  where
    loop stateVar conn problem simState = do
      -- Try reading from the shared state variable - if this thread's algorithm is not the
      -- one in the state variable, or the state is Paused, force another read to run by yielding
      -- If it's Running or Stepping, perform one step and send its result to the client.

      -- TODO: I don't like all this yielding. Feels like the thread should sleep instead
      -- and be woken up when it's supposed to do something.
      next <-
        readTVarIO stateVar >>= \(State currentAlgorithm runState) ->
          if currentAlgorithm /= thisThreadsAlgorithm
            then yield >> pure simState
            else case runState of
              Paused -> yield >> pure simState
              Stepping -> do
                let next = step problem simState
                    asStr = encode next
                liftIO $ sendTextData conn asStr
                atomically $ modifyTVar stateVar (\s -> s {runState = Paused})
                pure next
              Running -> do
                let next = step problem simState
                    asStr = encode next
                liftIO $ sendTextData conn asStr
                pure next
      loop stateVar conn problem next

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
        atomically $ writeTVar stateVar State {runState = Paused, currentAlgorithm = Surface}
      (Text "TSP" _, State _ _) -> do
        putStrLn "Starting TSP"
        atomically $ writeTVar stateVar State {runState = Paused, currentAlgorithm = TSP}
      (Text "Reservoir" _, State _ _) -> do
        putStrLn "Starting Reservoir"
        atomically $ writeTVar stateVar State {runState = Paused, currentAlgorithm = Reservoir}
      _ -> pure ()

-- Run the Warp server on port 8080
main :: IO ()
main = do
  putStrLn "Server running on port 8080"
  run 8080 $ cors (const $ Just corsPolicy) app
  where
    corsPolicy :: CorsResourcePolicy
    corsPolicy =
      simpleCorsResourcePolicy
        { corsOrigins = Nothing, -- allows all origins
          corsRequestHeaders = ["Content-Type"], -- allow only these request headers
          corsMethods = ["GET", "POST"] -- allow only these methods
        }
