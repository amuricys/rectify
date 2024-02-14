module Component.Parent where

import Prelude

import Component.Banner as Banner
import Component.Button as Button
import Component.Canvas as Canvas
import Component.Tabs as Tabs
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Halogen (Slot)
import Halogen as H
import Halogen.HTML as HH
import Type.Prelude (Proxy(..))

type ComponentState = Unit
data Action = Pause | Unpause | Step
data Output = SendPause | SendUnpause | SendStep

data Query a = CanvasQuery (Canvas.Query a)

component :: forall input m. MonadAff m => H.Component Query input Output m
component =
  H.mkComponent
    { initialState: const unit
    , render
    , eval: H.mkEval H.defaultEval { handleAction = handleAction, handleQuery = handleQuery }
    }

handleQuery :: forall m output q a. Query a -> H.HalogenM ComponentState Action (Slots q) output m (Maybe a)
handleQuery = case _ of
  CanvasQuery (Canvas.ReceiveSimState str a) -> do
    H.tell (Proxy @"canvas") 0 (Canvas.ReceiveSimState str)
    pure (Just a)

type Slots q =
  -- banner and button do not use a query
  ( banner :: Slot q Void Int
  , button :: Slot q Button.Output Int
  , tabs :: Slot q Tabs.Output Int
  , canvas :: Slot Canvas.Query Void Int
  )

render :: forall m s q. MonadEffect m => s -> H.ComponentHTML Action (Slots q) m
render _ =
  HH.div_
    [ HH.slot_ (Proxy @"banner") 0 Banner.component unit
    , HH.slot_ (Proxy @"tabs") 1 Tabs.component unit
    , HH.slot_ (Proxy @"canvas") 2 Canvas.component unit
    , HH.slot (Proxy @"button") 3 Button.component unit buttonAct
    ]
  where
  buttonAct :: Button.Output -> Action
  buttonAct = case _ of
    Button.SendPause -> Pause
    Button.SendUnpause -> Unpause
    Button.SendStep -> Step

handleAction :: forall slots m. MonadAff m => Action -> H.HalogenM ComponentState Action slots Output m Unit
handleAction = case _ of
  Pause -> H.raise $ SendPause
  Unpause -> H.raise $ SendUnpause
  Step -> H.raise $ SendStep