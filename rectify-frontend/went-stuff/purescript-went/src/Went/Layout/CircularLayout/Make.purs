module Went.Layout.CircularLayout.Make where

import Prelude

import GoJS.Layout (CircularLayout_, newCircularLayout)
import Went.Layout.Make (class LayoutM, MakeLayout, layout)

circularLayout :: forall (m :: Type -> Type) (b :: Type). LayoutM m => MakeLayout CircularLayout_ b -> m Unit
circularLayout = layout newCircularLayout
