module App.API where

import App.WsApp (wsApp)
import Control.Concurrent.STM (TVar, atomically, modifyTVar, newTVarIO, readTVarIO, writeTVar)
import Control.Exception (finally)
import Control.Monad (forever, when)
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
    (:>),
    type (:<|>) (..),
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

postHandler :: MyRequest -> Handler MyResponse
postHandler req = return $ MyResponse ("You sent: " ++ message req)

server :: Server ServantType
server = postHandler :<|> wsApp

app :: Application
app = serve (Proxy :: Proxy ServantType) server
  where
    ctx = EmptyContext