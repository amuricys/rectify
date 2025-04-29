{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

module SimulatedAnnealing.Surface.Surface2D where

import Data.Aeson (FromJSON, ToJSON (toJSON), Value (Object))
import Data.Aeson.KeyMap (fromList)
import Data.Bifunctor (first)
import Data.Finite (Finite, finite, getFinite)
import Data.Foldable (find)
import Data.Functor ((<&>))
import Data.Functor.Contravariant (Contravariant (contramap))
import Data.Kind (Type)
import Data.List (cycle, iterate, sortBy, tail, tails)
import Data.Map qualified as Map
import Data.Maybe (fromJust, isNothing)
import Data.Proxy (Proxy (..))
import Data.Type.Bool (If)
import Data.Type.Ord (type (<))
import Data.Vector qualified as Unsized
import Data.Vector.Sized (Vector, cons, drop, generate, index, (++), (//))
import Data.Vector.Sized qualified as V
import Debug.Pretty.Simple (pTraceShow)
import Effectful
import Effectful.Reader.Dynamic
import GHC.Generics (Generic)
import GHC.TypeLits
  ( CmpNat,
    KnownNat,
    Mod,
    Nat,
    OrderingI (LTI),
    SomeNat (SomeNat),
    cmpNat,
    natVal,
    someNatVal,
    type (+),
    type (-),
    type (<=?),
  )
import Random (RandomEff, nextDouble, nextInteger)
import SimulatedAnnealing (Probability (..), Problem (..))
import SimulatedAnnealing.Surface.Change
import SimulatedAnnealing.Surface.Circular
  ( AtLeastOneVector (AtLeastOneVector),
    Circular,
    Radius (..),
    circularGraph,
    maybeAddOnePoint,
    maybeRemoveOnePoint,
    toCircularLines,
  )
import SimulatedAnnealing.Surface.Index (Index)
import SimulatedAnnealing.Surface.Index qualified as Index
import SimulatedAnnealing.Surface.LinAlg (Point2D (Point2D), X (X), Y (Y), dist, linesIntersection)
import Prelude hiding (drop, (++))

-- Hmm.
class SurfaceData (dim :: Nat) where
  type Point dim :: Type
  volume :: forall o i. Surface (Point dim) (Point dim) -> Double
  perimeter :: forall o i. Surface (Point dim) (Point dim) -> Double

data Surface outerPoint innerPoint where
  Surface ::
    forall o i oprev iprev outerPoint innerPoint.
    (KnownNat o, KnownNat i, o ~ oprev + 1, i ~ iprev + 1) =>
    { outer :: Vector o outerPoint,
      inner :: Vector i innerPoint
    } ->
    Surface outerPoint innerPoint

instance (ToJSON a) => ToJSON (Vector n a) where
  toJSON :: (ToJSON a) => Vector n a -> Value
  toJSON = toJSON . V.toList

instance (ToJSON op, ToJSON ip) => ToJSON (Surface op ip) where
  toJSON :: (ToJSON op, ToJSON ip) => Surface op ip -> Value
  toJSON (Surface outer inner) = Object $ fromList [("tag", "SurfaceSolution"), ("values", toJSON [vals])]
    where
      vals =
        Object $
          fromList
            [ ("outer", toJSON outer),
              ("inner", toJSON inner)
            ]

newtype Compression = Compression {unCompression :: Double}
  deriving newtype (Show, Eq, Num, Ord, Fractional, FromJSON, ToJSON)

newtype Thickness = Thickness {unThickness :: Double}
  deriving newtype (Show, Eq, Num, Fractional, FromJSON, ToJSON)

------- Creation
circularSurface2D ::
  forall o i oprev iprev.
  (KnownNat o) =>
  (KnownNat i) =>
  (o ~ oprev + 1) =>
  (i ~ iprev + 1) =>
  X ->
  Y ->
  Radius ->
  Thickness ->
  Surface Point2D Point2D
circularSurface2D centerx centery radius thickness =
  Surface
    (circularGraph @o centerx centery radius)
    (circularGraph @i centerx centery (Radius $ unRadius radius - unThickness thickness))

surfAdded ::
  forall o i oprev iprev.
  (o ~ oprev + 1, i ~ iprev + 1, KnownNat o, KnownNat i) =>
  Double ->
  Vector o Point2D ->
  Vector i Point2D ->
  Surface Point2D Point2D
surfAdded thresh o i = case (maybeAddOnePoint thresh o, maybeAddOnePoint thresh i) of
  (AtLeastOneVector x, AtLeastOneVector y) -> Surface x y

surfRemoved ::
  forall o i oprev iprev.
  (o ~ oprev + 1, i ~ iprev + 1, KnownNat o, KnownNat i) =>
  Double ->
  Vector o Point2D ->
  Vector i Point2D ->
  Surface Point2D Point2D
surfRemoved thresh o i = case (maybeRemoveOnePoint thresh o, maybeRemoveOnePoint thresh i) of
  (AtLeastOneVector x, AtLeastOneVector y) -> Surface x y
