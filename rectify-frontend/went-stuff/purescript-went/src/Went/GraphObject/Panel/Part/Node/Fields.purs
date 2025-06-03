module Went.GraphObject.Panel.Part.Node.Fields where

import Prelude

import Effect (Effect)
import GoJS.GraphObject.Types (Link_, Node_, GraphObject_)
import Went.Geometry.Margin (Margin)
import Went.GraphObject.Fields.Pure (GraphObjectPureFields)
import Went.GraphObject.EnumValue.PortSpreading (PortSpreading)
import Went.GraphObject.Panel.Fields.Pure (PanelPureFields)
import Went.GraphObject.Panel.Part.Fields.Pure (PartPureFields)


{-
avoidable
avoidableMargin
isLinkLabel - Read-only
isTreeExpanded 
isTreeLeaf
labeledLink - Read-only, in a way
linkConnected 
linkDisconnected
linkValidation
linksConnected - Read-only
port - Read-only
portSpreading
ports - Read-only
treeExpandedChanged
wasTreeExpanded
-}

type NodePureFields (this :: Type) (a :: Row Type) =
  ( avoidable :: Boolean
  , avoidableMargin :: Margin
  , isTreeExpanded :: Boolean
  , isTreeLeaf :: Boolean
  , linkConnected :: this -> Link_ -> GraphObject_ -> Effect Unit
  , linkDisconnected :: this -> Link_ -> GraphObject_ -> Effect Unit
  , linkValidation :: this -> GraphObject_ -> Node_ -> GraphObject_ -> Link_ -> Effect Boolean
  , portSpreading :: PortSpreading
  , treeExpandedChanged :: this -> Effect Unit
  , wasTreeExpanded :: Boolean
  | a
  )

-- labeledLink, linksConnected

type NodeFields (this :: Type) (extraFields :: Row Type) =
  GraphObjectPureFields Node_
    ( PanelPureFields
        ( PartPureFields this
            ( NodePureFields this extraFields
            )
        )
    )
