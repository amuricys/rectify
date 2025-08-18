module Optimization.Renderer.Component where

import Prelude

import CSS as CSS
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Exception (throw)
import GoJS.Diagram (Diagram_)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.CSS as HCSS
import Halogen.HTML.Properties as HP
import Optimization.Problem as Problem
import Optimization.Renderer.Diagram as Renderer.Diagram

-- The renderer component for the optimization component holds a canvas that can either contain a GoJS diagram or a ThreeJS scene.
-- With an initialized state, new data comes in through queries from the optimization component, which holds the websocket connection.

-- Therefore we do not update the state unless a problem that merits a change in the DOM is received. Even though the state of the
-- underlying scene is changing continually, the render function should not be called in most cases; not even when a problem change that
-- still uses the same canvas. In fact the entire canvas component is going to remain identical from an HTML perspective, it's just
-- when the problem changes, we will want to tear down a GoJS diagram/ThreeJS scene and replace it with a new one.


data Scene_ = Scene_

data Action = Initialize | Finalize
data State = None | GoJS Diagram_ | ThreeJS Scene_



init :: String -> Problem.Problem -> Effect State
init divId prob = case Problem.rendererProblem prob of
  Problem.GoJS prob -> GoJS <$> Renderer.Diagram.initDiagram divId prob
  Problem.ThreeJS prob -> ThreeJS <$> throw "Not implemented"

update :: State -> Problem.Problem -> Effect Unit
update state prob = case state, Problem.rendererProblem prob of
  GoJS diagram, Problem.GoJS prob -> Renderer.Diagram.updateDiagram diagram prob
  ThreeJS scene, Problem.ThreeJS prob -> throw "Not implemented"
  _, _ -> throw "illegal state failed to be made unrepresentable"

canvasDivId :: String
canvasDivId = "canvasDiv"

data Query a 
  = ProblemStep Problem.Problem a
  | ProblemChange Problem.Problem a

component ∷ ∀ i m. MonadEffect m => H.Component Query i Void m
component = H.mkComponent
  { initialState: const None
  , render
  , eval: H.mkEval H.defaultEval
      { initialize = Just Initialize
      , finalize   = Just Finalize
      , handleAction = handleAction
      , handleQuery = handleQuery
      }
  }
  where 
    handleAction :: Action -> H.HalogenM State Action () Void m Unit
    handleAction = case _ of
      Initialize -> H.put None
      Finalize -> pure unit
    render :: State -> H.ComponentHTML Action () m
    render _ = HH.div
      [ HP.id canvasDivId
      , HCSS.style do
          CSS.height (CSS.pct 100.0)
      ]
      []
    handleQuery :: forall a. Query a -> H.HalogenM State Action () Void m (Maybe a)
    handleQuery = case _ of
      ProblemStep prob a -> do
        state <- H.get
        liftEffect $ update state prob
        pure (Just a)
      ProblemChange prob a -> do
        state <- liftEffect $ init canvasDivId prob
        H.put state
        pure (Just a)

