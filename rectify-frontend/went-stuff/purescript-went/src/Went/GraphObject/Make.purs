module Went.GraphObject.Make where

import Prelude

import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Data.Symbol (reflectSymbol)
import Effect (Effect)
import Effect.Class (liftEffect)
import GoJS.Diagram.AnimationTrigger.Constructors (newAnimationTrigger'')
import GoJS.GraphObject.Methods (bind_, trigger_)
import GoJS.GraphObject.Panel.Methods (add_)
import GoJS.GraphObject.Types (class IsGraphObject, class IsPanel, Group_)
import Prim.Row (class Union)
import Type.Data.List (type (:>), List', Nil')
import Went.Diagram.Animation.AnimationTrigger (class AnimationTriggerable)
import Went.FFI.Class (ffi)
import Went.GraphObject.Fields.All (class GraphObjectAllFields, class GraphObjectChildFields)
import Went.GraphObject.Panel.PanelType (PanelTypeTag)
import Went.Layout.Make (class LayoutM, layoutImp)
import Went.Model.Binding (class Bindable, bindingImp, bindingOfObjectImp)
import Went.Settable (class Settable, setImp)

newtype MakeGraphObject
  (bindable :: Row Type)
  (grObj :: Type)
  (hierarchy :: List' Type)
  (a :: Type) =
  MakeGraphObject (ReaderT grObj Effect a)

derive newtype instance Functor (MakeGraphObject bindable grObj hierarchy)
derive newtype instance Apply (MakeGraphObject bindable grObj hierarchy)
derive newtype instance Applicative (MakeGraphObject bindable grObj hierarchy)
derive newtype instance Monad (MakeGraphObject bindable grObj hierarchy)
derive newtype instance Bind (MakeGraphObject bindable grObj hierarchy)

newtype MadeGraphObject
  (bindable :: Row Type)
  (grObj :: Type)
  (a :: Type) =
  MadeGraphObject (Effect a)

derive newtype instance Functor (MadeGraphObject grObj bindable)
derive newtype instance Apply (MadeGraphObject grObj bindable)
derive newtype instance Applicative (MadeGraphObject grObj bindable)
derive newtype instance Monad (MadeGraphObject grObj bindable)
derive newtype instance Bind (MadeGraphObject grObj bindable)

class
  Monad m <=
  Hierarchy
    (child :: Type)
    (parent :: Type)
    (hierarchy :: List' Type)
    (bindable :: Row Type)
    m
  | m -> hierarchy parent bindable, hierarchy bindable -> m
  where
  wrapConstr :: Effect child -> m child
  prepare :: forall b. MakeGraphObject bindable child (parent :> hierarchy) b -> child -> m Unit
  embed :: child -> m child

instance emptyHierarchy ::
  Hierarchy
    g
    grObj
    Nil'
    bindable
    (MadeGraphObject bindable grObj) where
  wrapConstr = MadeGraphObject
  prepare (MakeGraphObject howToMake) g = MadeGraphObject $ runReaderT howToMake g *> pure unit
  embed = pure

else instance nonEmptyHierarchy ::
  ( IsGraphObject g
  , IsPanel parent
  ) =>
  Hierarchy
    g
    parent
    hierarchy
    bindable
    (MakeGraphObject bindable parent hierarchy) where
  wrapConstr = MakeGraphObject <<< liftEffect
  prepare (MakeGraphObject howToMake) ns = MakeGraphObject $ liftEffect (runReaderT howToMake ns *> pure unit)
  embed g = MakeGraphObject $ do
    parent <- ask
    void $ liftEffect $ add_ g parent
    pure g

graphObj
  :: forall m parent hierarchy bindable g b
   . Hierarchy g parent hierarchy bindable m
  => m g
  -> MakeGraphObject bindable g (parent :> hierarchy) b
  -> m g
graphObj wrappedConstr howToMake = do
  ns <- wrappedConstr
  prepare howToMake ns
  embed ns

instance (GraphObjectAllFields grObj settable) => Bindable (MakeGraphObject bindable grObj hierarchy) bindable settable where
  binding' ptgt prsc go back = MakeGraphObject $ bindingImp bind_ ptgt prsc go back
  bindingOfObject' ptgt src go back = MakeGraphObject $ bindingOfObjectImp bind_ ptgt src go back

instance (IsGraphObject grObj) => AnimationTriggerable (MakeGraphObject bindable grObj hierarchy) where
  animationTrigger' p r startCondition = MakeGraphObject $ do
    grObj <- ask
    animTrigger <- liftEffect $ newAnimationTrigger'' (reflectSymbol p) (ffi r) (ffi startCondition)
    void $ liftEffect $ trigger_ animTrigger grObj

-- Groups can have their own layouts, so layouts can be constructed in the
-- context of making a group. Because groups are made in the context of a
-- GraphObject, the instance is given here, otherwise it's orphaned.
instance LayoutM (MakeGraphObject bindable (PanelTypeTag panelType Group_) rest) where
  layout constructor = MakeGraphObject <<< layoutImp constructor

instance
  ( GraphObjectAllFields g settable
  , GraphObjectChildFields g hierarchy childSettable
  , Union settable childSettable settable'
  ) =>
  Settable (MakeGraphObject bindable g hierarchy) settable' where
  set fields = MakeGraphObject $ setImp fields
