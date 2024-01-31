{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Main where

import Prelude
import RIO (Text, MonadIO, TVar, forever, readTVarIO, atomically, writeTVar, newTVarIO, finally)

import Control.Concurrent (forkIO, yield)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson
import GHC.Generics (Generic)
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Handler.Warp qualified as Warp
import Network.Wai.Handler.WebSockets (websocketsOr)
import Network.Wai.Middleware.Cors
import Network.WebSockets (Connection, DataMessage (..), PendingConnection, acceptRequest, defaultConnectionOptions, forkPingThread, receiveDataMessage, sendDataMessage, sendTextData, withPingThread)

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

data State = Paused | Running

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
  -- Handle client messages in a new thread
  _ <- forkIO $ finally (handleClientMessages conn stateVar) (putStrLn "Client disconnected")
  -- Use the main thread to send messages to the client
  serverSendMessage conn stateVar

serverSendMessage :: Connection -> TVar State -> IO ()
serverSendMessage conn stateVar = forever $ do
  -- Try reading from the shared state variable - if the state is Paused
  -- force another thread to run by yielding. If it's Running, perform one step
  -- and send its result to the client.
  state <- readTVarIO stateVar
  case state of
    Paused -> do
      yield
    Running -> do
      putStrLn "Sending message to client"
      sendTextData conn ("Running" :: Text)

handleClientMessages :: Connection -> TVar State -> IO ()
handleClientMessages conn stateVar = 
  -- Ping the client every 30 seconds to keep the connection alive
  withPingThread conn 30 (putStrLn "Pinged!") $ forever $ do
    msg <- receiveDataMessage conn
    putStrLn $ "Received message from client: " ++ show msg
    state <- readTVarIO stateVar
    case (msg, state) of
      (Text "unpause" _, Paused) -> do
        putStrLn "Unpausing"
        atomically $ writeTVar stateVar Running
      (Text "pause" _, Running) -> do
        putStrLn "Pausing"
        atomically $ writeTVar stateVar Paused
      _ -> putStrLn "wth" >> pure ()

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
