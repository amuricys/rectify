module Went.Layout.LayeredDigraphLayout.Make where

import Prelude

import GoJS.Layout (LayeredDigraphLayout_, newLayeredDigraphLayout)
import Went.Layout.Make (class LayoutM, MakeLayout, layout)

layeredDigraphLayout :: forall (m :: Type -> Type) (b :: Type). LayoutM m => MakeLayout LayeredDigraphLayout_ b -> m Unit
layeredDigraphLayout = layout newLayeredDigraphLayout
