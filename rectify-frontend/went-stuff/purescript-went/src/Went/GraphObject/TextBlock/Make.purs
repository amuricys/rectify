module Went.GraphObject.TextBlock.Make where

import Prelude

import GoJS.GraphObject.Constructors (newTextBlock)
import GoJS.GraphObject.Types (TextBlock_)
import Type.Data.List (type (:>))
import Went.GraphObject.Make (MakeGraphObject, class Hierarchy, graphObj, wrapConstr)

textBlock
  :: forall bindable hierarchy parent m b
   . Hierarchy TextBlock_ parent hierarchy bindable m
  => String
  -> MakeGraphObject bindable TextBlock_ (parent :> hierarchy) b
  -> m TextBlock_
textBlock = graphObj <<< wrapConstr <<< newTextBlock
