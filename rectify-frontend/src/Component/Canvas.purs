module Component.Canvas where

import Prelude

import CSS as CSS
import CSS.Cursor as CSS.Cursor
import Data.Argonaut.Decode (decodeJson)
import Data.Argonaut.Parser (jsonParser)
import Data.Array (length, (..))
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Diagram as Diagram
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console (log)
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
data Query a = ReceiveSimState String a
type CanvasState = { diagram :: Maybe Diagram_ }

type Solution =
  { inner :: Array BackendPoint
  , outer :: Array BackendPoint
  }

type BackendSimState sol =
  { currentBeta :: Int
  , currentFitness :: Number
  , currentSolution :: sol
  }

type BackendPoint = { id :: Int, x :: Number, y :: Number }

fromSolution :: Solution -> { nodes :: Array (Record Diagram.NodeData), links :: Array (Record Diagram.LinkData) }
fromSolution { outer, inner } =
  { nodes: Array.concat
      [ map (toPoint "Outer" 0) outer
      , map (toPoint "Inner" outerLength) inner
      ]
  , links: Array.concat
      [ map (toLink "Outer" 0 (length outer - 1)) (0 .. (length outer - 1))
      , map (toLink "Inner" (length outer) (length outer + length inner - 1)) (length outer .. (length outer + length inner - 1))
      ]
  }
  where
  outerLength = length outer
  toLink cat first last i = { key: i, from: i, to: if i /= last then i + 1 else first, category: cat }
  toPoint cat idplus p = { key: idplus + p.id, loc: show (p.x * 200.0) <> " " <> show (p.y * 200.0), category: cat }

parse :: String -> Either String { nodes :: Array (Record Diagram.NodeData), links :: Array (Record Diagram.LinkData) }
parse = pure <<< fromSolution <<< _.currentSolution <=< lmap show <<< decodeJson @(BackendSimState Solution) <=< jsonParser

handleQuery :: forall m output a. MonadEffect m => Query a -> H.HalogenM CanvasState Action () output m (Maybe a)
handleQuery = case _ of
  ReceiveSimState msg a -> do
    -- Here we'll talk to GoJS
    { diagram } <- H.get
    case diagram <#> _model, parse msg of
      Just m, Right { nodes, links } -> do
        --        liftEffect $ log $ show nodes
        liftEffect $ m # mergeNodeDataArray_ nodes
        liftEffect $ m # mergeLinkDataArray_ links
        pure (Just a)
      _, Left err -> liftEffect (log err) *> pure (Just a)
      _, _ -> do
        pure $ Just a

component :: forall input m. MonadEffect m => H.Component Query input Void m
component = H.mkComponent
  { initialState: const { diagram: Nothing }
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
          CSS.height (CSS.px 1000.0)
          CSS.backgroundColor (CSS.white)
          CSS.position CSS.relative
          CSS.cursor CSS.Cursor.default
      ]
      []

handleAction :: forall slots m. MonadEffect m => Action -> H.HalogenM CanvasState Action slots Void m Unit
handleAction = case _ of
  Initialize -> do
    d <- liftEffect $ Went.make "myDiagramDiv" (Diagram.diag [] [])
    H.modify_ \s -> s { diagram = Just d }