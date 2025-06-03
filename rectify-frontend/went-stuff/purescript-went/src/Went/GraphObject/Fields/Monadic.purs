module Went.GraphObject.Fields.Monadic where

import Prelude

import Control.Monad.Reader (ask, runReaderT)
import Data.Symbol (class IsSymbol)
import Effect (Effect)
import Effect.Class (liftEffect)
import GoJS.GraphObject.Constructors (newContextMenu, newToolTip)
import GoJS.GraphObject.Types (class IsGraphObject, class IsPanel, Adornment_)
import GoJS.Unsafe.Set (setUnsafe)
import Prim.Row (class Cons)
import Record (insert)
import Type.Data.List (type (:>))
import Type.Prelude (Proxy(..))
import Went.GraphObject.Make (MakeGraphObject(..))
import Went.GraphObject.Panel.PanelType (Auto', PanelTypeTag(..), Vertical')

setter
  :: forall hierarchy bindable b g (singleton :: Row Type) (p :: Type) (@prop :: Symbol)
   . IsSymbol prop
  => IsPanel p
  => IsGraphObject g
  => Cons prop g () singleton
  => Effect g
  -> MakeGraphObject bindable g (p :> hierarchy) b
  -> MakeGraphObject bindable p hierarchy Unit
setter constructor (MakeGraphObject howToMake) = MakeGraphObject $ do
  n <- liftEffect constructor
  made <- liftEffect $ runReaderT (howToMake *> ask) n
  parent <- ask
  _ <- liftEffect $ setUnsafe parent (insert (Proxy @prop) made {})
  pure unit

toolTip
  :: forall bindable p b hierarchy
   . IsPanel p
  => MakeGraphObject bindable (PanelTypeTag Auto' Adornment_) (p :> hierarchy) b
  -> MakeGraphObject bindable p hierarchy Unit
toolTip = setter @"toolTip" (PanelTypeTag <$> newToolTip)

contextMenu
  :: forall bindable p b hierarchy
   . IsPanel p
  => MakeGraphObject bindable (PanelTypeTag Vertical' Adornment_) (p :> hierarchy) b
  -> MakeGraphObject bindable p hierarchy Unit
contextMenu = setter @"contextMenu" (PanelTypeTag <$> newContextMenu)
