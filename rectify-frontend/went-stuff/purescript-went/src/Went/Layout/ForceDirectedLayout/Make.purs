module Went.Layout.ForceDirectedLayout.Make where

import Prelude

import GoJS.Layout (ForceDirectedLayout_, newForceDirectedLayout)
import Went.Layout.Make (class LayoutM, MakeLayout, layout)

forceDirectedLayout :: forall (m :: Type -> Type) (b :: Type). LayoutM m => MakeLayout ForceDirectedLayout_ b -> m Unit
forceDirectedLayout = layout newForceDirectedLayout