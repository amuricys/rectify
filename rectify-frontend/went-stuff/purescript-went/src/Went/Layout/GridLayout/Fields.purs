module Went.Layout.GridLayout.Fields where

import Prelude

import GoJS.GraphObject.Types (Part_)
import GoJS.Layout.Types (GridLayout_)
import Went.Geometry.Size (Size)
import Went.Layout.Fields.Pure (LayoutPureFields)
import Went.Layout.EnumValue.GridAlignment (Alignment)
import Went.Layout.EnumValue.GridArrangement (Arrangement)
import Went.Layout.EnumValue.GridSorting (Sorting)

type GridPureFields =
  ( alignment :: Alignment
  , arrangement :: Arrangement
  , cellSize :: Size
  , comparer :: Part_ -> Part_ -> Number
  , sorting :: Sorting
  , spacing :: Size
  , wrappingColumn :: Int
  , wrappingWidth :: Number
  )

-- Grid has no network, so it's impossible to override makeNetwork for it.
type GridFields = LayoutPureFields GridLayout_ Void GridPureFields
