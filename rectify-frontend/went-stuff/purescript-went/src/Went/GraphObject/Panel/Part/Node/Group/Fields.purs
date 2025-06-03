module Went.GraphObject.Panel.Part.Node.Group.Fields where

import Prelude

import Effect (Effect)
import GoJS.GraphObject.Types (Group_, Part_)
import Went.GraphObject.Fields.Pure (GraphObjectPureFields)
import Went.GraphObject.Panel.Fields.Pure (PanelPureFields)
import Went.GraphObject.Panel.Part.Fields.Pure (PartPureFields)
import Went.GraphObject.Panel.Part.Node.Fields (NodePureFields)

{-
computesBoundsAfterDrag
computesBoundsIncludingLinks
computesBoundsIncludingLocation
handlesDragDropForMembers
isSubGraphExpanded
layout - Monadic
memberAdded
memberParts - Read-only
memberRemoved
memberValidation
placeholder - Read-only
subGraphExpandedChanged
ungroupable
wasSubGraphExpanded
-}

type GroupPureFields (a :: Row Type) =
  ( computesBoundsAfterDrag :: Boolean
  , computesBoundsIncludingLinks :: Boolean
  , computesBoundsIncludingLocation :: Boolean
  , handlesDragDropForMembers :: Boolean
  , isSubGraphExpanded :: Boolean
  , memberAdded :: Group_ -> Part_ -> Effect Unit
  , memberRemoved :: Group_ -> Part_ -> Effect Unit
  , memberValidation :: Group_ -> Part_ -> Boolean
  , subGraphExpandedChanged :: Group_ -> Effect Unit
  , ungroupable :: Boolean
  , wasSubGraphExpanded :: Boolean
  | a
  )

type GroupFields (extraFields :: Row Type) =
  GraphObjectPureFields Group_
    ( PanelPureFields
        ( PartPureFields Group_
            ( NodePureFields Group_
                ( GroupPureFields extraFields
                )
            )
        )
    )
