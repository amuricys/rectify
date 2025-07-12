module Component.Optimization where

import Prelude

import Backend.Optimization.Algorithm (AlgorithmData(..))
import Backend.Optimization.Algorithm as Algorithm
import Backend.Optimization.Problem as Problem
import Backend.TimeSeries (TemperatureData, EnergyData)
import CSS as CSS
import Component.Optimization.Diagram.Canvas as Canvas
import Component.Optimization.Diagram.TimeSeries as TimeSeries
import Component.Optimization.Button as Button
import Component.Optimization.Tabs as Tabs
import Control.Monad.Except (runExcept)
import Data.Argonaut.Decode (class DecodeJson, decodeJson, parseJson)
import Data.Argonaut.Decode.Generic (genericDecodeJson)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Foreign (Foreign, readString)
import Halogen (Slot)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.CSS as HCSS
import Halogen.Subscription as HS
import Type.Prelude (Proxy(..))
import Web.Event.EventTarget as EET
import Web.Socket.Event.EventTypes as WSET
import Web.Socket.Event.MessageEvent as ME
import Web.Socket.WebSocket as WS


-- ───────── payload from backend ──────────────────────────────────────
newtype Payload = Payload
  { solution :: Problem.Problem
  , algorithm :: Algorithm.AlgorithmData 
  }

derive instance genericPayload :: Generic Payload _

instance decodeJsonPayload :: DecodeJson Payload where
  decodeJson = genericDecodeJson


-- ───────── state / action / output ────────────────────────────────────
type State =
  { socket :: Maybe WS.WebSocket
  }

data Action
  = Initialize
  | Finalize
  | DataFrame Payload
  | ButtonClicked Button.RunCommand
  | TabsClicked Tabs.OptProblem

type Slots q =
  -- banner and button do not use a query
  ( banner :: Slot q Void Int
  , button :: Slot q Button.Output Int
  , tabs :: Slot q Tabs.Output Int
  , canvas :: Slot Canvas.Query Void Int
  , temperature :: Slot (TimeSeries.Query TemperatureData) Void Int
  , energy :: Slot (TimeSeries.Query EnergyData) Void Int
  )

inds :: { tabs :: Int , canvas :: Int , button :: Int , temperature :: Int , energy :: Int}
inds = {tabs: 0, canvas: 1, button: 2, temperature: 3, energy: 4}

_tabs :: Proxy "tabs"
_tabs = Proxy

_canvas :: Proxy "canvas"
_canvas = Proxy

_temperature :: Proxy "temperature"
_temperature = Proxy

_energy :: Proxy "energy"
_energy = Proxy

_button :: Proxy "button"
_button = Proxy

component ∷ ∀ q i m. MonadEffect m => H.Component q i Void m
component = H.mkComponent
  { initialState: const { socket: Nothing }
  , render
  , eval: H.mkEval H.defaultEval
      { initialize = Just Initialize
      , finalize   = Just Finalize
      , handleAction = handleAction
      }
  }

render :: forall m s q. MonadEffect m => s -> H.ComponentHTML Action (Slots q) m
render _ =
  HH.div_ -- Main container
    [ HH.slot _tabs inds.tabs Tabs.component unit tabsAct
    , HH.div -- Container for canvas and side graphs
        [ HCSS.style do
            CSS.display CSS.flex
            CSS.flexDirection CSS.row
            CSS.alignItems CSS.stretch -- Stretch items to fill container height
        ]
        [ HH.div -- Canvas container
            [ HCSS.style do
                CSS.flexGrow 1.0 -- Canvas takes remaining horizontal space
                CSS.height (CSS.px 500.0)
                CSS.width (CSS.px 500.0)
            ]
            [ HH.slot_ _canvas inds.canvas Canvas.component unit ]
        , HH.div -- Side graphs container
            [ HCSS.style do
                CSS.display CSS.flex
                CSS.flexDirection CSS.column
                CSS.width (CSS.px 250.0) -- Fixed width for the side panel
                CSS.height (CSS.px 500.0) -- Match canvas height
            ]
            [ 
            -- TODO: Implement a constant time queue before coming back to
            -- the time series diagrams. There's too many O(n) operations being done.
            
            --   HH.div -- Temperature container
            --     [ HCSS.style do
            --         CSS.height (CSS.px 250.0) -- Takes top 50% height
            --         CSS.border CSS.solid (CSS.px 1.0) CSS.black -- Basic border for visualization
            --         CSS.boxSizing CSS.borderBox
            --     ]
            --     [ HH.slot_ _temperature inds.temperature Temperature.component unit ]
            -- , HH.div -- Energy container
            --     [ HCSS.style do
            --         CSS.height (CSS.px 250.0) -- Takes top 50% height
            --         CSS.border CSS.solid (CSS.px 1.0) CSS.black -- Basic border for visualization
            --         CSS.boxSizing CSS.borderBox
            --     ]
            --     [ HH.slot_ _energy inds.energy Energy.component unit ]
            ]
        ]
    , HH.slot _button inds.button Button.component unit buttonAct -- Button below the canvas/graphs area
    ]
  where
  buttonAct :: Button.Output -> Action
  buttonAct (Button.Send runCmd) = ButtonClicked runCmd
  tabsAct :: Tabs.Output -> Action
  tabsAct (Tabs.Selected alg) = TabsClicked alg

-- ───────── websocket life-cycle  ──────────────────────────────────────
mkSocket :: Effect (Tuple WS.WebSocket (HS.Emitter Action))
mkSocket = do
  sock ← WS.create "ws://127.0.0.1:8080/ws" []
  { emitter, listener } ← HS.create
  -- forward each ws frame into the emitter
  l ← EET.eventListener \ev ->
        for_ (ME.fromEvent ev) \msg ->
          for_ (readHelper $ ME.data_ msg) \str ->
            HS.notify listener str
  EET.addEventListener WSET.onMessage l false (WS.toEventTarget sock)
  pure (Tuple sock emitter)
  where
    readHelper :: Foreign -> Either String Action
    readHelper frgn = case (lmap show <<< (decodeJson <=< parseJson)) =<< lmap show (runExcept (readString frgn)) of
      Right payload -> Right (DataFrame payload)
      Left err -> Left (show err)


handleAction :: forall m q. MonadEffect m => Action -> H.HalogenM State Action (Slots q) Void m Unit
handleAction = case _ of
  Initialize -> do
    -- 1. open socket + subscription
    (Tuple sock em) ← liftEffect mkSocket
    _ ← H.subscribe em
    -- 2. keep handle in state
    H.modify_ _ { socket = Just sock }

  Finalize -> do
    st ← H.get
    for_ st.socket \s -> liftEffect $ WS.close s

  DataFrame (Payload { solution, algorithm }) -> do
    H.tell _canvas inds.canvas <<< Canvas.ReceiveDiagramData $ solution
    case algorithm of
      SimulatedAnnealing algoData -> do
        H.tell _temperature inds.temperature (TimeSeries.Receive { beta: algoData.beta, betaCounter: algoData.betaCounter })
        H.tell _energy inds.energy (TimeSeries.Receive { fitness: algoData.fitness, betaCounter: algoData.betaCounter })
      GeneticAlgorithm algoData -> do
        pure unit -- H.tell _energy inds.energy (TimeSeries.Receive { fitness: algoData.fitness, betaCounter: algoData.betaCounter })

  ButtonClicked runCmd -> 
    withSocket_ \s -> liftEffect $ WS.sendString s (show runCmd)

  TabsClicked alg -> pure unit
  where 
  withSocket_ f = do
    st ← H.get
    for_ st.socket f

