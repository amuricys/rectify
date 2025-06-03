module Went.Tool.ClickCreatingTool where

import Prelude

import Effect (Effect)
import GoJS.Tool.Types (ClickCreatingTool_)
import Went.FFI.Override (Override)
import GoJS.Geometry.Types (Point_)
import Went.Tool (ToolPureFields)

type ClickCreatingToolPureFields (nodeData :: Row Type) =
  ( archetypeNodeData :: Record nodeData
  , isDoubleClick :: Boolean
  , isGridSnapEnabled :: Boolean
  -- Extra overridable method
  , insertPart :: Override ClickCreatingTool_ (Point_ -> Effect Unit)
  )

type ClickCreatingToolFields nodeData = ToolPureFields ClickCreatingTool_ (ClickCreatingToolPureFields nodeData)
