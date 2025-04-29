module Component.Canvas where

import Prelude

import Algorithm (Algorithm)
import Backend (DiagramData(..))
import CSS as CSS
import CSS.Cursor as CSS.Cursor
import Data.Maybe (Maybe(..))
import Diagram.Surface as Diagram.Surface
import Effect.Class (class MonadEffect, liftEffect)
import GoJS.Diagram (Diagram_, _model)
import GoJS.Model (mergeLinkDataArray_, mergeNodeDataArray_)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.CSS as HCSS
import Halogen.HTML.Properties as HP
import Went.Diagram.Make as Went

-- Maybe use https://hackage.haskell.org/package/purescript-bridge for this
data SimState solution metric = SimState
  { currentSolution :: solution
  , currentBeta :: Int
  , currentFitness :: metric
  }
data Action = Initialize
data Query a = ReceiveDiagramData DiagramData a | AlgorithmChange DiagramData Algorithm a
data CanvasState = SurfaceDiagram Diagram_ | TSPDiagram Diagram_ | NoDiagramYet

updateDiagram :: forall output m a. MonadEffect m => Diagram_ -> DiagramData -> a -> H.HalogenM CanvasState Action () output m (Maybe a)
updateDiagram diagram (DiagramData { nodes, links }) a = do
  let m = diagram # _model 
  liftEffect $ m # mergeNodeDataArray_ nodes
  liftEffect $ m # mergeLinkDataArray_ links
  pure (Just a)

handleQuery :: forall m a. MonadEffect m => Query a -> H.HalogenM CanvasState Action () Void m (Maybe a)
handleQuery = case _ of
  -- Here we'll talk to GoJS
  AlgorithmChange state alg a -> do
    initDiagram state alg
    pure $ Just a
  ReceiveDiagramData msg a -> do
    H.get >>= case _ of
      SurfaceDiagram d -> updateDiagram d msg a
      TSPDiagram d -> updateDiagram d msg a
      NoDiagramYet -> pure $ Just a

component :: forall input m. MonadEffect m => H.Component Query input Void m
component = H.mkComponent
  { initialState: const NoDiagramYet
  , render
  , eval: H.mkEval $ H.defaultEval
      { handleAction = handleAction
      , handleQuery = handleQuery
      , initialize = Just Initialize
      }
  }
  where
  render :: forall mon slot s. MonadEffect mon => s -> H.ComponentHTML Action slot mon
  render _ =
    -- This below div is for rendering of the diagram.
    HH.div
      [ HP.id "myDiagramDiv"
      , HCSS.style do
          CSS.flexGrow 1.0
          CSS.height (CSS.px 500.0)
          CSS.backgroundColor (CSS.white)
          CSS.position CSS.relative
          CSS.cursor CSS.Cursor.default
      ]
      []

initDiagram :: forall m. MonadEffect m => DiagramData -> Algorithm -> H.HalogenM CanvasState Action () Void m Unit
initDiagram (DiagramData initialState) _alg = do
  -- TODO: 1. Clear existing diagram before this, then 2. Make different diagram for different algorithms
  d <- liftEffect $ Went.make "myDiagramDiv" (Diagram.Surface.diag initialState.nodes initialState.links)
  H.modify_ <<< const $ SurfaceDiagram d

handleAction :: forall slots m. MonadEffect m => Action -> H.HalogenM CanvasState Action slots Void m Unit
handleAction = case _ of
  Initialize -> do
    d <- liftEffect $ Went.make "myDiagramDiv" (Diagram.Surface.diag [] [])
    H.modify_ <<< const $ SurfaceDiagram d