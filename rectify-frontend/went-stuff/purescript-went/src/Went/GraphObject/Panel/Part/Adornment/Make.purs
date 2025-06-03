module Went.GraphObject.Panel.Part.Adornment.Make where

import Prelude

import Control.Monad.Reader (ask, runReaderT)
import GoJS.GraphObject.Constructors (newAdornment)
import GoJS.GraphObject.Types (Adornment_)
import Type.Data.List (Nil')
import Went.GraphObject.Make (MadeGraphObject(..), MakeGraphObject(..))
import Went.GraphObject.Panel.AsString (class AsString, asString)
import Went.GraphObject.Panel.PanelType (PanelType, PanelTypeTag(..), unPanelTypeTag)

adornment
  :: forall (@panelType :: PanelType) bindable b
   . AsString panelType
  => MakeGraphObject bindable (PanelTypeTag panelType Adornment_) Nil' b
  -> MadeGraphObject bindable Adornment_ Adornment_
adornment (MakeGraphObject howToMake) = MadeGraphObject $ do
  n <- PanelTypeTag <$> (newAdornment $ asString @panelType)
  runReaderT (unPanelTypeTag <$> (howToMake *> ask)) n
