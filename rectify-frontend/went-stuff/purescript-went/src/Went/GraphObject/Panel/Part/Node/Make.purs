module Went.GraphObject.Panel.Part.Node.Make where

import Prelude

import Control.Monad.Reader (ask, runReaderT)
import GoJS.GraphObject.Constructors (newNode)
import GoJS.GraphObject.Types (Node_)
import Type.Data.List (Nil')
import Went.GraphObject.Make (MadeGraphObject(..), MakeGraphObject(..))
import Went.GraphObject.Panel.AsString (class AsString, asString)
import Went.GraphObject.Panel.PanelType (PanelType, PanelTypeTag(..), unPanelTypeTag)

node
  :: forall (@panelType :: PanelType) bindable b
   . AsString panelType
  => MakeGraphObject bindable (PanelTypeTag panelType Node_) Nil' b
  -> MadeGraphObject bindable Node_ Node_
node (MakeGraphObject howToMake) = MadeGraphObject $ do
  n <- PanelTypeTag <$> (newNode $ asString @panelType)
  runReaderT (unPanelTypeTag <$> (howToMake *> ask)) n
