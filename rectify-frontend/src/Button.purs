module Button where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE

foreign import debuglog :: forall a. a -> Effect Unit

-- Define message and state types
data Action = ButtonClicked
data Output = SendPause | SendUnpause
data ServerState = Running | Paused
derive instance Generic ServerState _
instance Show ServerState where
  show = genericShow

data Query a = ReceiveMessage String a


type ComponentState = {serverState :: ServerState, receivedAmt :: Int}

buttonComponent :: forall input m. MonadAff m => H.Component Query input Output m
buttonComponent =
  H.mkComponent
    { initialState: const {serverState: Paused, receivedAmt: 0}
    , render
    , eval: H.mkEval H.defaultEval { handleAction = handleAction, handleQuery = handleQuery }
    }

render :: forall m. ComponentState -> H.ComponentHTML Action () m
render state =
  HH.div_
    [ HH.button [ HE.onClick \_ -> ButtonClicked ] [ HH.text (show state.serverState <> ", msgCount: " <> show state.receivedAmt) ]
    ]

handleAction :: forall slots m. MonadAff m => Action -> H.HalogenM ComponentState Action slots Output m Unit
handleAction = case _ of
  ButtonClicked -> do
    {serverState} <- H.get
    case serverState of
      Running -> do
        H.modify_ _ { serverState = Paused }
        H.raise $ SendPause
      Paused -> do
        H.modify_ _ { serverState = Running }
        H.raise $ SendUnpause

handleQuery :: forall m a. Query a -> H.HalogenM ComponentState Action () Output m (Maybe a)
handleQuery = case _ of
  ReceiveMessage _msg a -> do
    H.modify_ \s -> s { receivedAmt = s.receivedAmt + 1 }
    pure (Just a)

    -- resp <- liftAff $ AX.post
    --     AXW.driver
    --     ResponseFormat.string
    --     "http://localhost:8080/mypostendpoint"
    --     (Just $ RequestBody.json $ encodeJson { message: "Hello server" })
