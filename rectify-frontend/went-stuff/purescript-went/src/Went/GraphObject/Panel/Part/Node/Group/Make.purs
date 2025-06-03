module Went.GraphObject.Panel.Part.Node.Group.Make where

import Prelude

import Control.Monad.Reader (ask, runReaderT)
import GoJS.GraphObject.Constructors (newGroup)
import GoJS.GraphObject.Types (Group_)
import Type.Data.List (Nil')
import Went.GraphObject.Make (MadeGraphObject(..), MakeGraphObject(..))
import Went.GraphObject.Panel.AsString (class AsString, asString)
import Went.GraphObject.Panel.PanelType (PanelType, PanelTypeTag(..), unPanelTypeTag)

group
  :: forall (@panelType :: PanelType) bindable b
   . AsString panelType
  => MakeGraphObject bindable (PanelTypeTag panelType Group_) Nil' b
  -> MadeGraphObject bindable Group_ Group_
group (MakeGraphObject howToMake) = MadeGraphObject $ do
  n <- PanelTypeTag <$> (newGroup $ asString @panelType)
  runReaderT (unPanelTypeTag <$> (howToMake *> ask)) n
