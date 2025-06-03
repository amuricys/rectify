module Went.Layout.CircularLayout.Fields where

import GoJS.Layout.Types (CircularLayout_, CircularNetwork_)
import Went.Geometry.Point (Point)
import Went.Layout.Fields.Pure (LayoutPureFields)
import Went.Layout.EnumValue.CircularArrangement (Arrangement)
import Went.Layout.EnumValue.CircularSorting (Sorting)
import Went.Layout.EnumValue.Direction (Direction)
import Went.Layout.EnumValue.NodeDiameterFormula (NodeDiameterFormula)

type CircularPureFields =
  ( actualCenter :: Point
  , actualSpacing :: Number
  , actualXRadius :: Number
  , actualYRadius :: Number
  , arrangement :: Arrangement
  , aspectRatio :: Number
  , direction :: Direction
  , nodeDiameterFormula :: NodeDiameterFormula
  , radius :: Number
  , sorting :: Sorting
  , spacing :: Number
  , startAngle :: Number
  , sweepAngle :: Number
  )

type CircularFields = LayoutPureFields CircularLayout_ CircularNetwork_ CircularPureFields
