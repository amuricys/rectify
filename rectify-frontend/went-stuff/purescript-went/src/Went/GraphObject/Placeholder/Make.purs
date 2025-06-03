module Went.GraphObject.Placeholder.Make where

import GoJS.GraphObject.Constructors (newPlaceholder)
import GoJS.GraphObject.Types (Placeholder_)
import Type.Data.List (type (:>))
import Went.GraphObject.Make (MakeGraphObject, class Hierarchy, graphObj, wrapConstr)

placeholder
  :: forall bindable hierarchy parent m b
   . Hierarchy Placeholder_ parent hierarchy bindable m
  => MakeGraphObject bindable Placeholder_ (parent :> hierarchy) b
  -> m Placeholder_
placeholder = graphObj (wrapConstr newPlaceholder)
