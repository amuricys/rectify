module Went.Layout.TreeLayout.Make where

import Prelude

import GoJS.Layout (TreeLayout_, newTreeLayout)
import Went.Layout.Make (class LayoutM, MakeLayout, layout)

treeLayout :: forall (m :: Type -> Type) (b :: Type). LayoutM m => MakeLayout TreeLayout_ b -> m Unit
treeLayout = layout newTreeLayout
