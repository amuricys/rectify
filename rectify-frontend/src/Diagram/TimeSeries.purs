module Diagram.TimeSeries where

import Prelude

import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import GoJS.Debug (ffilog, trace)
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
pastDataPointCategory :: String
pastDataPointCategory = "PastDataPoint"

presentDataPointCategory :: String
presentDataPointCategory = "PresentDataPoint"

-- Link categories
linkCategory :: String
linkCategory = "Link"

-- Raw data point type (used by the component managing the diagram)
type TimeValuePoint = { time :: Number, value :: Number }

type NodeData =
  ( key :: String -- Using String keys for flexibility (e.g., "origin", "max_y", "t_123.45")
  , category :: String
  , loc :: String -- GoJS location string "x y"
  , time :: Number
  , value :: Number
  )

type LinkData =
  ( key :: String -- Unique link key (e.g., "link_123.45")
  , from :: String -- Key of the 'from' node
  , to :: String -- Key of the 'to' node
  , category :: String
  )

mkDataPointNode :: Int -> Number -> Int -> Number -> Record NodeData
mkDataPointNode x y time value =
  { key: "t_" <> show time
  , category: if time == 0 then presentDataPointCategory else pastDataPointCategory
  , loc: trace $ show (toNumber x) <> " " <> show y
  , time: toNumber time
  , value: value
  }

mkLink :: Int -> Int -> Record LinkData
mkLink t1 t2 =
  { key: "link_" <> show t1 <> "_" <> show t2
  , from: "t_" <> show t1
  , to: "t_" <> show t2
  , category: linkCategory
  }

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

invisibleNodeTemplate :: MadeGraphObject NodeData Node_ Node_
invisibleNodeTemplate = node @Auto' do
  set { desiredSize: SizeBoth 1.0 }
  binding @"location" @"loc" (Just (Point.parse_ <<< trace)) Nothing

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
  dataPointToolTip

dataLinkTemplate :: MadeGraphObject LinkData Link_ Link_
dataLinkTemplate = link do
  shape None $ do
    set { stroke: "grey", strokeWidth: 1.0 }

diag :: Array (Record NodeData) -> Array (Record LinkData) -> MakeDiagram NodeData LinkData Diagram_ Unit
diag initialNodeData initialLinkData = do
  attach
    { "undoManager.isEnabled": false
    }
  addNodeTemplate pastDataPointCategory invisibleNodeTemplate
  addNodeTemplate presentDataPointCategory dataNodeTemplate
  addLinkTemplate linkCategory dataLinkTemplate

  graphLinksModel do
    set
      { nodeDataArray: initialNodeData
      , linkDataArray: initialLinkData
      , linkKeyProperty: Property "key"
      , linkFromKeyProperty: Property "from"
      , linkToKeyProperty: Property "to"
      }