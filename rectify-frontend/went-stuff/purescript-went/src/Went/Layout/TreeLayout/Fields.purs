module Went.Layout.TreeLayout.Fields where

import GoJS.Layout.Types (TreeLayout_, TreeNetwork_)
import Went.Layout.Fields.Pure (LayoutPureFields)
import Went.Layout.EnumValue.TreeAlignment (Alignment)
import Went.Layout.EnumValue.TreeArrangement (Arrangement)
import Went.Layout.EnumValue.TreeStyle (TreeStyle)

type TreePureFields =
  ( treeStyle :: TreeStyle
  , arrangement :: Arrangement
  , layerSpacing :: Number
  , nodeSpacing :: Number
  , alternateAngle :: Number
  , alternateLayerSpacing :: Number
  , alternateAlignment :: Alignment
  , alternateNodeSpacing :: Number
  , angle :: Number
  )

type TreeFields = LayoutPureFields TreeLayout_ TreeNetwork_ TreePureFields
