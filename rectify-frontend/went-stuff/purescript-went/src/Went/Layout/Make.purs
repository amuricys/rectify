module Went.Layout.Make where

import Prelude

import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Effect (Effect)
import Effect.Class (liftEffect)
import GoJS.Layout (class IsLayout)
import GoJS.Unsafe.Set (setUnsafe)
import Went.Layout.Fields.All (class LayoutFields)
import Went.Settable (class Settable, setImp)

newtype MakeLayout (ffiType :: Type) (a :: Type) = MakeLayout (ReaderT ffiType Effect a)

derive newtype instance Functor (MakeLayout ffiType)
derive newtype instance Apply (MakeLayout ffiType)
derive newtype instance Applicative (MakeLayout ffiType)
derive newtype instance Bind (MakeLayout ffiType)
derive newtype instance Monad (MakeLayout ffiType)

-- | This class exists to support setting layouts in multiple monadic contexts
-- Both MakeDiagram and MakeGraphObject (Group_ :>) support layouts. It also
-- makes this module export a more reasonable API by not requiring that each
-- implementer of the typeclass give all types of layouts.
class LayoutM (m :: Type -> Type) where
  layout :: forall l b. IsLayout l => Effect l -> (MakeLayout l b) -> m Unit

layoutImp :: forall (b :: Type) (l :: Type) (parent :: Type). Effect l -> MakeLayout l b -> ReaderT parent Effect Unit
layoutImp constructor (MakeLayout howToMakeLayout) = do
  l <- liftEffect constructor
  void $ liftEffect $ runReaderT howToMakeLayout l
  parent <- ask
  liftEffect $ setUnsafe parent { layout: l }

instance (LayoutFields ffiType settable) => Settable (MakeLayout ffiType) settable where
  set fields = MakeLayout $ setImp fields