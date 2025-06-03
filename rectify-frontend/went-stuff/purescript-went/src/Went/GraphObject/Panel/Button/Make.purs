module Went.GraphObject.Panel.Button.Make where

import Prelude

import GoJS.GraphObject.Constructors (newButton)
import GoJS.GraphObject.Types (Button_)
import Type.Data.List (type (:>))
import Went.GraphObject.Make (class Hierarchy, MakeGraphObject, graphObj, wrapConstr) 
import Went.GraphObject.Panel.AsString (class AsString, asString)
import Went.GraphObject.Panel.Button.ButtonType (ButtonTypeTag(..), unButtonTypeTag)

button
  :: forall @buttonType @panelType bindable hierarchy parent m b
   . AsString panelType
  => AsString buttonType
  => Hierarchy (ButtonTypeTag buttonType panelType Button_) parent hierarchy bindable m
  => MakeGraphObject bindable (ButtonTypeTag buttonType panelType Button_) (parent :> hierarchy) b
  -> m Button_
button = (unButtonTypeTag <$> _) <<< graphObj (wrapConstr $ ButtonTypeTag <$> newButton (asString @buttonType) (asString @panelType))
