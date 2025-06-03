module Went.GraphObject.Panel.Make where

import Prelude

import GoJS.GraphObject.Constructors (newPanel)
import GoJS.GraphObject.Types (class IsPanel, Panel_)
import Type.Data.List (type (:>))
import Went.GraphObject.Make (MakeGraphObject, graphObj, wrapConstr)
import Went.GraphObject.Panel.AsString (class AsString, asString)
import Went.GraphObject.Panel.PanelType (PanelTypeTag(..), unPanelTypeTag)

panel
  :: forall @panelType bindable p b hierarchy
   . IsPanel p
  => AsString panelType
  => MakeGraphObject bindable (PanelTypeTag panelType Panel_) (p :> hierarchy) b
  -> MakeGraphObject bindable p hierarchy Panel_
panel = (unPanelTypeTag <$> _) <<< graphObj (wrapConstr $ PanelTypeTag <$> newPanel (asString @panelType))

-- button
--   :: forall @buttonType @panelType bindable p b hierarchy
--    . IsPanel p
--   => AsString panelType
--   => AsString buttonType
--   => MakeGraphObject bindable (ButtonTypeTag buttonType panelType Button_) (p :> hierarchy) b
--   -> MakeGraphObject bindable p hierarchy Unit
-- button = makerPanel $ (ButtonTypeTag <$> newButton (asString @buttonType) (asString @panelType))
