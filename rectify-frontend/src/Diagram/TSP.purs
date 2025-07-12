module Diagram.TSP where

import Prelude

import Data.Maybe (Maybe(..))
import GoJS.Diagram (Diagram_)
import GoJS.Geometry.Point.Properties (_x, _y)
import GoJS.Geometry.Point.Static as Point
import GoJS.GraphObject.Types (class IsPanel, Node_)
import GoJS.Key (KeyProperty(..))
import Went.Diagram.Make (MakeDiagram, addLinkTemplate, addNodeTemplate, attach)
import Went.Geometry (Margin(..), Size(..))
import Went.Geometry as Spot
import Went.GraphObject (Auto', MadeGraphObject, MakeGraphObject, Table', link, node, panel, shape, textBlock, toolTip)
import Went.GraphObject.Shape.Figure (Figure(..))
import Went.Model (binding, graphLinksModel)
import Went.Settable (set)

type NodeData =
  ( key :: Int
  , category :: String
  , loc :: String
  )

type LinkData =
  ( key :: Int
  , from :: Int
  , to :: Int
  , category :: String
  )

toolTipStyle :: forall p g. IsPanel p => String -> MakeGraphObject NodeData p g Unit
toolTipStyle surf =
  toolTip $
    panel @Table' do
      set { defaultAlignment: Spot.left, margin: MarginEach { top: 0.0, right: 6.0, bottom: 0.0, left: 6.0 } }
      textBlock surf do
        set { row: 0 }
      textBlock "" $ do
        set { row: 1 }
        binding @"text" @"key" (Just \key -> "id: " <> show key) Nothing
      textBlock "" $ do
        set { row: 2 }
        binding @"text" @"loc" (Just \loc -> "x: " <> show (Point.parse_ loc # _x)) Nothing
      textBlock "" $ do
        set { row: 3 }
        binding @"text" @"loc" (Just \loc -> "y: " <> show (Point.parse_ loc # _y)) Nothing

outerNodeTemplate :: MadeGraphObject NodeData Node_ Node_
outerNodeTemplate = node @Auto' do
  shape Circle $ do
    set { fill: "blanchedalmond", desiredSize: SizeBoth 1.25, strokeWidth: 0.07 }
  binding @"location" @"loc" (Just Point.parse_) Nothing
  toolTipStyle "Outer"

innerNodeTemplate :: MadeGraphObject NodeData Node_ Node_
innerNodeTemplate = node @Auto' do
  shape Circle $ do
    set { fill: "burlywood", desiredSize: SizeBoth 1.25, strokeWidth: 0.07 }
  binding @"location" @"loc" (Just Point.parse_) Nothing
  toolTipStyle "Inner"

diag :: Array (Record NodeData) -> Array (Record LinkData) -> MakeDiagram NodeData LinkData Diagram_ Unit
diag initialNodeData initialLinkData = do
  attach { "undoManager.isEnabled": true }
  addNodeTemplate "Outer" outerNodeTemplate
  addNodeTemplate "Inner" innerNodeTemplate
  addLinkTemplate "Outer" $ link
    $ shape None
    $ set { stroke: "black", strokeWidth: 0.1 }
  addLinkTemplate "Inner" $ link
    $ shape None
    $ set { stroke: "black", strokeWidth: 0.1 }
  graphLinksModel do
    set
      { nodeDataArray: initialNodeData
      , linkDataArray: initialLinkData
      , linkKeyProperty: Property "key"
      }