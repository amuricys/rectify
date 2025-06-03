module Went.GraphObject.Placeholder.Fields where

import GoJS.GraphObject.Types (Placeholder_)

import Went.Geometry.Margin (Margin)
import Went.GraphObject.Fields.Pure (GraphObjectPureFields)


type PlaceholderPureFields =
  ( padding :: Margin
  )

type PlaceholderFields = GraphObjectPureFields Placeholder_ PlaceholderPureFields
