module Went.GraphObject.Panel.Part.Fields.Monadic where

import Prelude

import Control.Monad.Reader (ask)
import Effect.Class (liftEffect)
import GoJS.GraphObject.Types (class IsPart, Adornment_)
import GoJS.Unsafe.Set (setUnsafe)
import Went.GraphObject.Make (MadeGraphObject(..), MakeGraphObject(..))

-- adornments, containingGroup
selectionAdornmentTemplate
  :: forall bindable g hierarchy p
   . IsPart p
  => MadeGraphObject bindable Adornment_ g
  -> MakeGraphObject bindable p hierarchy Unit
selectionAdornmentTemplate (MadeGraphObject howToHaveMade) = MakeGraphObject $ do
  finished <- liftEffect howToHaveMade
  parent <- ask
  liftEffect $ setUnsafe parent { selectionAdornmentTemplate: finished }

resizeAdornmentTemplate
  :: forall bindable g hierarchy p
   . IsPart p
  => MadeGraphObject bindable Adornment_ g
  -> MakeGraphObject bindable p hierarchy Unit
resizeAdornmentTemplate (MadeGraphObject howToHaveMade) = MakeGraphObject $ do
  finished <- liftEffect howToHaveMade
  parent <- ask
  liftEffect $ setUnsafe parent { resizeAdornmentTemplate: finished }

rotateAdornmentTemplate
  :: forall bindable g hierarchy p
   . IsPart p
  => MadeGraphObject bindable Adornment_ g
  -> MakeGraphObject bindable p hierarchy Unit
rotateAdornmentTemplate (MadeGraphObject howToHaveMade) = MakeGraphObject $ do
  finished <- liftEffect howToHaveMade
  parent <- ask
  liftEffect $ setUnsafe parent { rotateAdornmentTemplate: finished }
