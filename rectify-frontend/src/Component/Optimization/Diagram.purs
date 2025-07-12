module Component.Optimization.Diagram where

import Prelude

import CSS as CSS
import Data.Maybe (Maybe(..))
import Effect.Class (class MonadEffect, liftEffect)
import GoJS.Diagram (Diagram_, _model)
import GoJS.Model (mergeLinkDataArray_, mergeNodeDataArray_)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.CSS as HCSS
import Halogen.HTML.Properties as HP
import Went.Diagram.Make (MakeDiagram)
import Went.Diagram.Make as Went

data Action = Initialize

type State s = { state :: s, diagram :: Maybe Diagram_ }

newtype DiagramData nodeData linkData = DiagramData { nodes :: Array (Record nodeData), links :: Array (Record linkData) }
instance Semigroup (DiagramData nodeData linkData) where
  append (DiagramData { nodes: nodes1, links: links1 }) (DiagramData { nodes: nodes2, links: links2 }) =
    DiagramData { nodes: nodes1 <> nodes2, links: links1 <> links2 }

type MkDiagram nodeData linkData = Array (Record nodeData) -> Array (Record linkData) -> MakeDiagram nodeData linkData Diagram_ Unit

diagramComponent
  :: forall nodeData linkData input m query s
   . MonadEffect m
  => s 
  -> String
  -> MkDiagram nodeData linkData
  -> (forall a.query a -> H.HalogenM (State s) Action () Void m (Maybe a))
  -> H.Component query input Void m
diagramComponent s diagramDivId mkDiagram handleQuery = H.mkComponent
  { initialState: const { state: s, diagram: Nothing }
  , render
  , eval: H.mkEval $ H.defaultEval
      { handleAction = handleAction diagramDivId mkDiagram
      , handleQuery = handleQuery
      , initialize = Just Initialize
      }
  }
  where
  --  render :: forall mon slot s. MonadEffect mon => s -> H.ComponentHTML Action slot mon
  render _ =
    -- This below div is for rendering of the diagram.
    -- GoJS adds a canvas element to the div, all it needs
    -- is the id and an explicit size (height or width).
    HH.div
      [ HP.id diagramDivId
      , HCSS.style do
          CSS.height (CSS.pct 100.0)
      ]
      [  ]

handleAction :: forall m nodeData linkData s. MonadEffect m => String -> MkDiagram nodeData linkData -> Action -> H.HalogenM (State s) Action () Void m Unit
handleAction diagramDivId mkDiagram = case _ of
  Initialize -> initDiagram diagramDivId (DiagramData { nodes: [], links: [] }) mkDiagram

updateDiagram :: forall m nodeData linkData s. MonadEffect m => Diagram_ -> DiagramData nodeData linkData -> H.HalogenM (State s) Action () Void m Unit
updateDiagram diagram (DiagramData { nodes, links }) = do
  let m = diagram # _model
  liftEffect $ m # mergeNodeDataArray_ nodes
  liftEffect $ m # mergeLinkDataArray_ links

initDiagram :: forall m nodeData linkData s. MonadEffect m => String -> DiagramData nodeData linkData -> MkDiagram nodeData linkData -> H.HalogenM (State s) Action () Void m Unit
initDiagram divId (DiagramData initialState) mkDiagram = do
  -- TODO: 1. Clear existing diagram before this, then 2. Make different diagram for different algorithms
  d <- liftEffect $ Went.make divId (mkDiagram initialState.nodes initialState.links)
  H.modify_ _ { diagram = Just d } 