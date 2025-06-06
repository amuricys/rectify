module Component.Parent where

import Prelude

import Algorithm (Algorithm)
import Backend (Solution(..))
import Backend as Backend
import Backend.Surface (fromSurfaceSolution)
import Backend.TSP (fromTSPSolutionData)
import Backend.TimeSeries (TemperatureData, EnergyData)
import CSS as CSS
import Component.Banner as Banner
import Component.Button as Button
import Component.Diagram.Canvas as Canvas
import Component.Diagram.Energy as Energy
import Component.Diagram.Temperature as Temperature
import Component.Diagram.TimeSeries as TimeSeries
import Component.Tabs as Tabs
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Halogen (Slot)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.CSS as HCSS
import RunCommand (RunCommand)
import Type.Prelude (Proxy(..))

type ComponentState = Unit

data Action = RunAction RunCommand | AlgorithmChangeAction Algorithm
data Output = SendRunCommand RunCommand | SendAlgorithmChange Algorithm

data Query a 
  = PropagateBackendData (Backend.SimState Backend.Solution) a 
  | AlgorithmChange (Backend.SimState Backend.Solution) Algorithm a

-- Constants to avoid mixing up indices and proxies
inds :: { banner :: Int , button :: Int , canvas :: Int , tabs :: Int , temperature :: Int , energy :: Int }
inds = {banner: 0, tabs: 1, canvas: 2, button: 3, temperature: 4, energy: 5}

_banner :: Proxy "banner"
_banner = Proxy

_tabs :: Proxy "tabs"
_tabs = Proxy

_canvas :: Proxy "canvas"
_canvas = Proxy

_button :: Proxy "button"
_button = Proxy

_temperature :: Proxy "temperature"
_temperature = Proxy

_energy :: Proxy "energy"
_energy = Proxy

component :: forall input m. MonadAff m => H.Component (Query) input Output m
component =
  H.mkComponent
    { initialState: const unit
    , render
    , eval: H.mkEval H.defaultEval { handleAction = handleAction, handleQuery = handleQuery }
    }


solData str = case str.solution of 
  SurfaceSolution surfSolData -> fromSurfaceSolution surfSolData
  TSPSolution tspSolData -> fromTSPSolutionData tspSolData

handleQuery :: forall m output q a. Query a -> H.HalogenM ComponentState Action (Slots q) output m (Maybe a)
handleQuery q = case q of
  PropagateBackendData str a -> do
    H.tell _canvas inds.canvas <<< Canvas.ReceiveDiagramData $ solData str
    H.tell _temperature inds.temperature (TimeSeries.Receive { beta: str.beta, betaCounter: str.betaCounter })
    H.tell _energy inds.energy (TimeSeries.Receive { fitness: str.fitness, betaCounter: str.betaCounter })
    pure (Just a)
  AlgorithmChange str alg a -> do
    H.tell _canvas inds.canvas (Canvas.AlgorithmChange (solData str) alg)
    pure (Just a)

type Slots q =
  -- banner and button do not use a query
  ( banner :: Slot q Void Int
  , button :: Slot q Button.Output Int
  , tabs :: Slot q Tabs.Output Int
  , canvas :: Slot Canvas.Query Void Int
  , temperature :: Slot (TimeSeries.Query TemperatureData) Void Int
  , energy :: Slot (TimeSeries.Query EnergyData) Void Int
  )

render :: forall m s q. MonadEffect m => s -> H.ComponentHTML Action (Slots q) m
render _ =
  HH.div_ -- Main container
    [ HH.slot_ _banner inds.banner Banner.component unit -- Banner remains at the top
    , HH.slot _tabs inds.tabs Tabs.component unit tabsAct -- Tabs below banner
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
  buttonAct (Button.Send runCmd) = RunAction runCmd
  tabsAct :: Tabs.Output -> Action
  tabsAct (Tabs.Selected alg) = AlgorithmChangeAction alg

handleAction :: forall slots m. MonadAff m => Action -> H.HalogenM ComponentState Action slots Output m Unit
handleAction = case _ of
  RunAction cmd -> H.raise $ SendRunCommand cmd
  AlgorithmChangeAction alg -> do
    H.raise $ SendAlgorithmChange alg
