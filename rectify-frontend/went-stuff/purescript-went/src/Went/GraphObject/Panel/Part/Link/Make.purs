module Went.GraphObject.Panel.Part.Link.Make where

import Prelude

import Control.Monad.Reader (ask, runReaderT)
import GoJS.GraphObject.Constructors (newLink)
import GoJS.GraphObject.Types (Link_)
import Type.Data.List (Nil')
import Went.GraphObject.Make (MadeGraphObject(..), MakeGraphObject(..))
import Went.GraphObject.Panel.PanelType (Link', PanelTypeTag(..), unPanelTypeTag)

link
  :: forall bindable b
   . MakeGraphObject bindable (PanelTypeTag Link' Link_) Nil' b
  -> MadeGraphObject bindable Link_ Link_
link (MakeGraphObject howToMake) = MadeGraphObject $ do
  n <- PanelTypeTag <$> newLink
  runReaderT (unPanelTypeTag <$> (howToMake *> ask)) n
