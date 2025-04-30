module Diagram.TimeSeries where

import Prelude

import Data.Maybe (Maybe(..))
import GoJS.Diagram (Diagram_, _initialAutoScale)
import GoJS.Diagram.Properties (_allowMove, _allowZoom)
import GoJS.Geometry.Point.Static as Point
import GoJS.GraphObject.Types (class IsPanel, Link_, Node_)
import GoJS.Key (KeyProperty(..))
import Went.Diagram.Make (MakeDiagram, addLinkTemplate, addNodeTemplate, attach)
import Went.Geometry (Margin(..), Size(..))
import Went.Geometry as Spot
import Went.GraphObject (Auto', MadeGraphObject, MakeGraphObject, Table', link, node, panel, shape, textBlock, toolTip)
import Went.GraphObject.Shape.Figure (Figure(..))
import Went.Model (binding, graphLinksModel)
import Went.Settable (set)

-- Node categories
invisibleCategory :: String
invisibleCategory = "InvisibleAxis"

dataPointCategory :: String
dataPointCategory = "DataPoint"

-- Link categories
dataLinkCategory :: String
dataLinkCategory = "DataLink"

-- Raw data point type (used by the component managing the diagram)
type TimeValuePoint = { time :: Number, value :: Number }

-- Node data structure for GoJS model
type NodeData =
  ( key :: String -- Using String keys for flexibility (e.g., "origin", "max_y", "t_123.45")
  , category :: String
  , loc :: String -- GoJS location string "x y"
  , time :: Number
  , value :: Number
  )

-- Link data structure for GoJS model
type LinkData =
  ( key :: String -- Unique link key (e.g., "link_123.45")
  , from :: String -- Key of the 'from' node
  , to :: String -- Key of the 'to' node
  , category :: String
  )

-- Helper to create a data point node record
mkDataPointNode :: TimeValuePoint -> String -> Record NodeData
mkDataPointNode tv locString =
  { key: "t_" <> show tv.time
  , category: dataPointCategory
  , loc: locString
  , time: tv.time
  , value: tv.value
  }

-- Helper to create a link record between two time points
mkLink :: Number -> Number -> Record LinkData
mkLink t1 t2 =
  { key: "link_" <> show t1 <> "_" <> show t2
  , from: "t_" <> show t1
  , to: "t_" <> show t2
  , category: dataLinkCategory
  }

-- Tooltip template for data points
dataPointToolTip :: forall p g. IsPanel p => MakeGraphObject NodeData p g Unit
dataPointToolTip =
  toolTip $
    panel @Table' do
      set { defaultAlignment: Spot.left, margin: MarginAll 4.0 }
      textBlock "Time: " $ do
        set { row: 0, column: 0 }
      textBlock "" $ do
        set { row: 0, column: 1 }
        binding @"text" @"time" (Just show) Nothing
      textBlock "Value: " $ do
        set { row: 1, column: 0 }
      textBlock "" $ do
        set { row: 1, column: 1 }
        binding @"text" @"value" (Just show) Nothing

-- Node template for invisible axis markers
invisibleNodeTemplate :: MadeGraphObject NodeData Node_ Node_
invisibleNodeTemplate = node @Auto' do
  set { desiredSize: SizeBoth 1.0 } -- Minimal size
  binding @"location" @"loc" (Just Point.parse_) Nothing
-- We don't add any visual elements (shape, textBlock)

-- Node template for visible data points
dataNodeTemplate :: MadeGraphObject NodeData Node_ Node_
dataNodeTemplate = node @Auto' do
  shape Circle $ do
    set
      { fill: "dodgerblue"
      , stroke: "blue"
      , strokeWidth: 0.5
      , desiredSize: SizeBoth 5.0 -- Small circle
      }
  binding @"location" @"loc" (Just Point.parse_) Nothing
  dataPointToolTip -- Attach the tooltip

-- Link template for connecting data points
dataLinkTemplate :: MadeGraphObject LinkData Link_ Link_
dataLinkTemplate = link do
  shape None $ do -- Shape follows the link path
    set { stroke: "grey", strokeWidth: 1.0 }

-- Main diagram definition function
diag :: Array (Record NodeData) -> Array (Record LinkData) -> MakeDiagram NodeData LinkData Diagram_ Unit
diag initialNodeData initialLinkData = do
  attach
    { "undoManager.isEnabled": false
    }
  addNodeTemplate invisibleCategory invisibleNodeTemplate
  addNodeTemplate dataPointCategory dataNodeTemplate
  addLinkTemplate dataLinkCategory dataLinkTemplate

  graphLinksModel do
    set
      { nodeDataArray: initialNodeData
      , linkDataArray: initialLinkData
      , linkKeyProperty: Property "key"
      , linkFromKeyProperty: Property "from"
      , linkToKeyProperty: Property "to"
      }