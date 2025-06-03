module Went.GraphObject.Picture.Make where

import Prelude

import GoJS.GraphObject.Constructors (newPicture)
import GoJS.GraphObject.Types (Picture_)
import Type.Data.List (type (:>))
import Went.GraphObject.Make (MakeGraphObject, class Hierarchy, graphObj, wrapConstr)

picture
  :: forall bindable hierarchy parent m b
   . Hierarchy Picture_ parent hierarchy bindable m
  => String
  -> MakeGraphObject bindable Picture_ (parent :> hierarchy) b
  -> m Picture_
picture = graphObj <<< wrapConstr <<< newPicture
