module Went.GraphObject.Panel.Part.Make where

import Prelude

import Control.Monad.Reader (ask, runReaderT)
import GoJS.GraphObject.Constructors (newPart)
import GoJS.GraphObject.Types (Part_)
import Type.Data.List (Nil')
import Went.GraphObject.Make (MadeGraphObject(..), MakeGraphObject(..))
import Went.GraphObject.Panel.AsString (class AsString, asString)
import Went.GraphObject.Panel.PanelType (PanelType, PanelTypeTag(..), unPanelTypeTag)

part
  :: forall (@panelType :: PanelType) bindable b
   . AsString panelType
  => MakeGraphObject bindable (PanelTypeTag panelType Part_) Nil' b
  -> MadeGraphObject bindable Part_ Part_
part (MakeGraphObject howToMake) = MadeGraphObject $ do
  n <- PanelTypeTag <$> (newPart $ asString @panelType)
  runReaderT (unPanelTypeTag <$> (howToMake *> ask)) n
