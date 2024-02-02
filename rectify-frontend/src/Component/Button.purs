module Component.Button where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE

-- Define message and state types
data Action = PauseClicked | StepClicked
data Output = SendPause | SendUnpause | SendStep
data ServerState = Running | Paused

derive instance Generic ServerState _
instance Show ServerState where
  show = genericShow

type ComponentState = { serverState :: ServerState }

component :: forall query input m. H.Component query input Output m
component =
  H.mkComponent
    { initialState: const { serverState: Paused }
    , render
    , eval: H.mkEval H.defaultEval { handleAction = handleAction }
    }

render :: forall m. ComponentState -> H.ComponentHTML Action () m
render state =
  HH.div_ $
    [ HH.button [ HE.onClick \_ -> PauseClicked ] [ HH.text (show state.serverState) ]
    ] <> case state.serverState of
      Paused -> [ HH.button [ HE.onClick \_ -> StepClicked ] [ HH.text "Step" ] ]
      Running -> []

handleAction :: forall slots m. Action -> H.HalogenM ComponentState Action slots Output m Unit
handleAction = case _ of
  PauseClicked -> do
    { serverState } <- H.get
    case serverState of
      Running -> do
        H.modify_ _ { serverState = Paused }
        H.raise $ SendPause
      Paused -> do
        H.modify_ _ { serverState = Running }
        H.raise $ SendUnpause
  StepClicked -> do
    H.raise $ SendStep

-- resp <- liftAff $ AX.post
--     AXW.driver
--     ResponseFormat.string
--     "http://localhost:8080/mypostendpoint"
--     (Just $ RequestBody.json $ encodeJson { message: "Hello server" })
