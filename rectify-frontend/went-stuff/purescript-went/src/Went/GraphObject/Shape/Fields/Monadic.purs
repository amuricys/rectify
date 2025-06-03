module Went.GraphObject.Shape.Fields.Monadic where

import Prelude

import Control.Monad.Reader (ask)
import Effect.Class (liftEffect)
import GoJS.GraphObject.Types (class IsGraphObject, Shape_)
import GoJS.Unsafe.Set (setUnsafe)
import Went.GraphObject.Make (MadeGraphObject(..), MakeGraphObject(..))

pathPattern
  :: forall bindable g hierarchy
   . IsGraphObject g
  => MadeGraphObject bindable g g
  -> MakeGraphObject bindable Shape_ hierarchy Unit
pathPattern (MadeGraphObject howToHaveMade) = MakeGraphObject $ do
  finished <- liftEffect howToHaveMade
  parent <- ask
  liftEffect $ setUnsafe parent { pathPattern: finished }
