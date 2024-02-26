module Component.Parent where

import Prelude

import Algorithm (Algorithm)
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
import RunCommand (RunCommand)
import Type.Prelude (Proxy(..))

type ComponentState = Unit

data Action = RunAction RunCommand | AlgorithmChangeAction Algorithm
data Output = SendRunCommand RunCommand | SendAlgorithmChange Algorithm

data Query a = CanvasQuery (Canvas.Query a)

-- Constants to avoid mixing up indices and proxies
inds :: { banner :: Int , button :: Int , canvas :: Int , tabs :: Int }
inds = {banner: 0, tabs:1, canvas: 2, button: 3}

_banner :: Proxy "banner"
_banner = Proxy

_tabs :: Proxy "tabs"
_tabs = Proxy

_canvas :: Proxy "canvas"
_canvas = Proxy

_button :: Proxy "button"
_button = Proxy

component :: forall input m. MonadAff m => H.Component Query input Output m
component =
  H.mkComponent
    { initialState: const unit
    , render
    , eval: H.mkEval H.defaultEval { handleAction = handleAction, handleQuery = handleQuery }
    }

handleQuery :: forall m output q a. Query a -> H.HalogenM ComponentState Action (Slots q) output m (Maybe a)
handleQuery (CanvasQuery q) = case q of
  Canvas.ReceiveSimState str a -> do
    H.tell _canvas inds.canvas (Canvas.ReceiveSimState str)
    pure (Just a)
  Canvas.AlgorithmChange initialState alg a -> do
    H.tell _canvas inds.canvas (Canvas.AlgorithmChange initialState alg)
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
    [ HH.slot_ _banner inds.banner Banner.component unit
    , HH.slot _tabs inds.tabs Tabs.component unit tabsAct
    , HH.slot_ _canvas inds.canvas Canvas.component unit
    , HH.slot _button inds.button Button.component unit buttonAct
    ]
  where
  buttonAct :: Button.Output -> Action
  buttonAct (Button.Send runCmd) = RunAction runCmd
  tabsAct :: Tabs.Output -> Action
  tabsAct (Tabs.Selected alg) = AlgorithmChangeAction alg

handleAction :: forall slots m. MonadAff m => Action -> H.HalogenM ComponentState Action slots Output m Unit
handleAction = case _ of
  RunAction cmd -> H.raise $ SendRunCommand cmd
  AlgorithmChangeAction alg -> do
    H.raise $ SendAlgorithmChange alg
