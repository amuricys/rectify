module Went.Tool.RelinkingTool where


import GoJS.Tool.Types (RelinkingTool_)
import Went.Tool (ToolPureFields)
import Went.Tool.LinkingBaseTool (LinkingBaseToolPureFields)

type RelinkingToolPureFields :: forall k. Row k
type RelinkingToolPureFields =
  ()

type RelinkingToolFields :: forall k. k -> Row Type
type RelinkingToolFields nodeData = ToolPureFields RelinkingTool_ (LinkingBaseToolPureFields RelinkingToolPureFields)
