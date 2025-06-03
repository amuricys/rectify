module Went.Layout.GridLayout.Make where

import Prelude

import GoJS.Layout (GridLayout_, newGridLayout)
import Went.Layout.Make (class LayoutM, MakeLayout, layout)

gridLayout :: forall (m :: Type -> Type) (b :: Type). LayoutM m => MakeLayout GridLayout_ b -> m Unit
gridLayout = layout newGridLayout
