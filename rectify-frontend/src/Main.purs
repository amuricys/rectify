module Main where

import Prelude

import Button as Button
import Control.Coroutine as CR
import Control.Coroutine.Aff as CRA
import Control.Monad.Except (runExcept)
import Data.Either (either)
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Foreign (readString, unsafeToForeign)
import Halogen as H
import Halogen.Aff as HA
import Halogen.Subscription as HS
import Halogen.VDom.Driver (runUI)
import Web.Event.EventTarget as EET
import Web.Socket.Event.EventTypes as WSET
import Web.Socket.Event.MessageEvent as ME
import Web.Socket.WebSocket as WS

-- Based on https://github.com/purescript-halogen/purescript-halogen/tree/master/examples/driver-websockets

wsProducer :: WS.WebSocket -> CR.Producer String Aff Unit
wsProducer socket = CRA.produce \emitter -> do
  listener <- EET.eventListener \ev -> do
    for_ (ME.fromEvent ev) \msgEvent -> 
      for_ (readHelper (ME.data_ msgEvent)) \msg -> do
        CRA.emit emitter msg
  EET.addEventListener
    WSET.onMessage
    listener
    false
    (WS.toEventTarget socket)
  where
    readHelper :: forall a. a -> Maybe String
    readHelper = either (const Nothing) Just <<< runExcept <<< readString <<< unsafeToForeign

wsConsumer :: (forall a. Button.Query a -> Aff (Maybe a)) -> CR.Consumer String Aff Unit
wsConsumer query = CR.consumer \msg -> do
  void $ query $ H.mkTell $ Button.ReceiveMessage msg
  pure Nothing

wsSender :: WS.WebSocket -> Button.Output -> Effect Unit
wsSender socket = case _ of
  Button.SendPause ->
    WS.sendString socket "pause"
  Button.SendUnpause ->
    WS.sendString socket "unpause"

main :: Effect Unit
main = do
  wsConnection <- WS.create "ws:127.0.0.1:8080/ws" []
  HA.runHalogenAff do
    body <- HA.awaitBody
    io <- runUI Button.buttonComponent unit body
    -- Subscribe to all output messages from our component
    _ <- H.liftEffect $ HS.subscribe io.messages $ wsSender wsConnection
    CR.runProcess (wsProducer wsConnection CR.$$ wsConsumer io.query)
