module Went.GraphObject.Shape.Make where

import Prelude

import GoJS.GraphObject.Constructors (newShape)
import GoJS.GraphObject.Types (Shape_)
import Type.Data.List (type (:>))
import Went.GraphObject.Make (MakeGraphObject, class Hierarchy, graphObj, wrapConstr)
import Went.GraphObject.Shape.Figure (Figure)

shape
  :: forall bindable hierarchy parent m b
   . Hierarchy Shape_ parent hierarchy bindable m
  => Figure
  -> MakeGraphObject bindable Shape_ (parent :> hierarchy) b
  -> m Shape_
shape = graphObj <<< wrapConstr <<< newShape <<< show
