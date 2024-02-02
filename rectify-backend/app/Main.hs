{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Main where

import Prelude
import RIO (Text, MonadIO, TVar, forever, readTVarIO, atomically, writeTVar, newTVarIO, finally, ReaderT, runReaderT, MonadReader (ask), MonadUnliftIO (withRunInIO))

import Control.Monad.IO.Class (liftIO)
import Data.Aeson
import GHC.Generics (Generic)
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Handler.Warp qualified as Warp
import Network.Wai.Handler.WebSockets (websocketsOr)
import Network.Wai.Middleware.Cors
import Network.WebSockets (Connection, DataMessage (..), PendingConnection, acceptRequest, defaultConnectionOptions, forkPingThread, receiveDataMessage, sendDataMessage, sendTextData, withPingThread)
import UnliftIO.Concurrent (forkIO, yield)

import Servant
  ( Application,
    Handler,
    JSON,
    Post,
    Proxy (..),
    ReqBody,
    serve,
    type (:<|>) (..),
    (:<|>),
    type (:>), Server, Context ((:.), EmptyContext), serveWithContext,
  )
import Servant.API.WebSocket
import Surface (circularSurface2D, modifySurf, sched, freeEnergy, acceptanceProbability, seed, Surface (..))
import SimulatedAnnealing (Problem(..), step, SimState (..))
import RIO.Text (pack)
import qualified RIO.ByteString.Lazy as RIO.ByteString

-- Define a type for your API
type ServantType =
  "mypostendpoint" :> ReqBody '[JSON] MyRequest :> Post '[JSON] MyResponse
  :<|> 
  "ws" :> WebSocketPending

data MyRequest = MyRequest {message :: String}
  deriving (Eq, Show, Generic)

instance FromJSON MyRequest
instance ToJSON MyRequest

data MyResponse = MyResponse {reply :: String}
  deriving (Eq, Show, Generic)

instance FromJSON MyResponse
instance ToJSON MyResponse

data State = Paused | Running | Stepping

data Ctx = Ctx
  { stateVar :: TVar State
  , conn :: Connection
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
  stateVar <- newTVarIO Paused
  let ctx = Ctx {stateVar, conn}
  handleAndSend ctx
  where 
    handleAndSend ctx = do
      -- Handle client messages in a new thread
      _ <- forkIO $ finally (runReaderT handleClientMessages ctx) (putStrLn "Client disconnected")
      -- Use the main thread to send messages to the client
      runReaderT serverSendMessage ctx

serverSendMessage :: ReaderT Ctx IO ()
serverSendMessage = do
  Ctx {stateVar, conn} <- ask
  let surf = circularSurface2D @150 @150 0 0 1 0.2
      initialGrayMatterArea = 0.0
      problem = Problem {
        initial = const surf, 
        neighbor = modifySurf (0.04, -0.04), 
        fitness = freeEnergy initialGrayMatterArea, 
        schedule = sched,
        acceptance = \_ _ -> acceptanceProbability
      }
  loop stateVar conn problem SimState {currentSolution = surf, currentFitness = freeEnergy initialGrayMatterArea surf, currentBeta = 0, gen = seed}
  where 
    loop stateVar conn problem simState = do
      -- Try reading from the shared state variable - if the state is Paused
      -- force another thread to run by yielding. If it's Running, perform one step
      -- and send its result to the client.
      next <- readTVarIO stateVar >>= \case 
        Paused -> yield >> pure simState
        Stepping -> do
          let next = step problem simState
              asStr = encode next
          liftIO . RIO.ByteString.putStrLn $ "Sending message to client: " <> asStr
          liftIO $ sendTextData conn asStr
          atomically $ writeTVar stateVar Paused
          pure next
        Running -> do
          let next = step problem simState
              asStr = encode next
          -- liftIO . RIO.ByteString.putStrLn $ "Sending message to client: " <> asStr
          liftIO $ sendTextData conn asStr
          pure next
      loop stateVar conn problem next

handleClientMessages :: ReaderT Ctx IO ()
handleClientMessages = do
  Ctx {stateVar, conn} <- ask
  -- Ping the client every 30 seconds to keep the connection alive
  liftIO $ withPingThread conn 30 (putStrLn "Pinged!") $ forever $ do
    msg <- receiveDataMessage conn
    putStrLn $ "Received message from client: " ++ show msg
    state <- readTVarIO stateVar
    case (msg, state) of
      (Text "unpause" _, Paused) -> do
        putStrLn "Unpausing"
        atomically $ writeTVar stateVar Running
      (Text "step" _, Paused) -> do
        putStrLn "Stepping"
        atomically $ writeTVar stateVar Stepping
      (Text "pause" _, Running) -> do
        putStrLn "Pausing"
        atomically $ writeTVar stateVar Paused
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
