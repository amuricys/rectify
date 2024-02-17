{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Main where

import Control.Concurrent.STM
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
import Problem
import Debug.Pretty.Simple
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
import Servant.API.WebSocket
import SimulatedAnnealing (Problem (..), SimState (..), step)
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

data State = Paused | Running | Stepping

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
  stateVar <- newTVarIO Paused
  let ctx = Ctx {stateVar, conn}
  handleAndSend ctx
  where
    handleAndSend ctx = do
      -- Handle client messages in a new thread
      _ <- forkIO $ finally (handleClientMessages ctx) (putStrLn "Client disconnected")
      -- Use the main thread to send messages to the client
      serverSendMessage ctx

serverSendMessage :: Ctx -> IO ()
serverSendMessage Ctx {stateVar, conn} = do
  let problem = Problem.surfaceProblem @250 @250 @7 200 5
      is = problem.initial seed
      f = problem.fitness is
  loop stateVar conn problem SimState {currentSolution = is, currentFitness = f, currentBeta = 0, gen = seed}
  where
    loop stateVar conn problem simState = do
      -- Try reading from the shared state variable - if the state is Paused
      -- force another thread to run by yielding. If it's Running, perform one step
      -- and send its result to the client.
      next <-
        readTVarIO stateVar >>= \case
          Paused -> yield >> pure simState
          Stepping -> do
            let next = step problem simState
                asStr = encode next
            liftIO $ sendTextData conn asStr
            atomically $ writeTVar stateVar Paused
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
      (Text "Unpause" _, Paused) -> do
        putStrLn "Unpausing"
        atomically $ writeTVar stateVar Running
      (Text "Step" _, Paused) -> do
        putStrLn "Stepping"
        atomically $ writeTVar stateVar Stepping
      (Text "Pause" _, Running) -> do
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
