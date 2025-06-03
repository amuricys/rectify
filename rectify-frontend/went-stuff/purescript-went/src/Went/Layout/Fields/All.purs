module Went.Layout.Fields.All where

import GoJS.Layout.Types (CircularLayout_, ForceDirectedLayout_, GridLayout_, LayeredDigraphLayout_, TreeLayout_)
import Went.Layout.GridLayout.Fields (GridFields)
import Went.Layout.CircularLayout.Fields (CircularFields)
import Went.Layout.ForceDirectedLayout.Fields (ForceDirectedFields)
import Went.Layout.LayeredDigraphLayout.Fields (LayeredDigraphFields)
import Went.Layout.TreeLayout.Fields (TreeFields)

class LayoutFields (ffiType :: Type) (fields :: Row Type) | ffiType -> fields

instance LayoutFields GridLayout_ GridFields
instance LayoutFields CircularLayout_ CircularFields
instance LayoutFields ForceDirectedLayout_ ForceDirectedFields
instance LayoutFields LayeredDigraphLayout_ LayeredDigraphFields
instance LayoutFields TreeLayout_ TreeFields
