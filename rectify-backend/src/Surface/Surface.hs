{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

module Surface.Surface where

import Data.Aeson (FromJSON, ToJSON (toJSON), Value (Object))
import Data.Aeson.KeyMap (fromList)
import Data.Bifunctor (first)
import Data.Finite (Finite, finite, getFinite)
import Data.Foldable (find)
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
import GHC.Generics (Generic)
import GHC.TypeLits (CmpNat, KnownNat, Mod, Nat, OrderingI (LTI), SomeNat (SomeNat), cmpNat, natVal, someNatVal, type (+), type (-), type (<=?))
import SimulatedAnnealing (Probability (..), Problem (..))
import Surface.Change
import Surface.Circular (Circular, Compression (Compression), Radius (..), circularGraph, maybeAddOnePoint, toCircularLines)
import Surface.Index (Index)
import Surface.Index qualified as Index
import Surface.LinAlg (Point2D (Point2D), X (X), Y (Y), dist, linesIntersection)
import System.Random.SplitMix (SMGen, nextDouble, nextInteger)
import Unsafe.Coerce (unsafeCoerce)
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

newtype Thickness = Thickness {unThickness :: Double}
  deriving newtype (Show, Eq, Num, Fractional, FromJSON, ToJSON)

------- Creation
circularSurface2D :: forall o i oprev iprev. (KnownNat o) => (KnownNat i) => (o ~ oprev + 1) => (i ~ iprev + 1) => X -> Y -> Radius -> Thickness -> Surface Point2D Point2D
circularSurface2D centerx centery radius thickness =
  Surface
    (circularGraph @o centerx centery radius)
    (circularGraph @i centerx centery (Radius $ unRadius radius - unThickness thickness))

withSurface ::
  Surface outer inner ->
  ( forall o i.
    (KnownNat o, KnownNat i) =>
    Vector o outer ->
    Vector i inner ->
    r
  ) ->
  r
withSurface (Surface out inn) f = f out inn

dirtyfmap :: (c a) => (c b) => (forall a. (c a) => a -> d) -> Either a b -> d
dirtyfmap f e = case e of
  Left x -> f x
  Right x -> f x


surfOutof :: forall o i oprev iprev. (o ~ oprev + 1, i ~ iprev + 1, KnownNat o, KnownNat i) => Vector o Point2D -> Vector i Point2D -> Surface Point2D Point2D
surfOutof o i = case ( maybeAddOnePoint 10.0 o,  maybeAddOnePoint 10.0 i) of
  -- This is needed because the case expression
  (Left x, Left y) -> pTraceShow ("ol: " <> show (V.length x) <> ", il: " <> show (V.length y)) Surface x y
  (Left x, Right y) -> pTraceShow ("ol: " <> show (V.length x) <> ", il: " <> show (V.length y)) Surface x y
  (Right x, Left y) -> pTraceShow ("ol: " <> show (V.length x) <> ", il: " <> show (V.length y)) Surface x y
  (Right x, Right y) -> pTraceShow ("ol: " <> show (V.length x) <> ", il: " <> show (V.length y)) Surface x y

--------- Modification
modifySurf ::
  forall avg.
  (KnownNat avg) =>
  (Double, Double) -> -- range of random change to x and y
  SMGen ->
  Surface Point2D Point2D ->
  (SMGen, Surface Point2D Point2D)
modifySurf (high, low) gen s@(Surface (outer :: Vector o Point2D) (inner :: Vector i Point2D)) =
  let -- Choose a random point on the outer surface (- 1 because nextInteger interval is closed)
      (i, newgen) = first finite $ nextInteger 0 (natVal (Proxy @o) - 1) gen
      -- Choose a random change to x and y
      (xChange, newgen') = first (\x -> X $ x * 2 * high + low) (nextDouble newgen)
      (yChange, newgen'') = first (\y -> Y $ y * 2 * high + low) (nextDouble newgen')
      c = Change xChange yChange
      smoothedOuterChanges = smooth @8 i c linear
      smoothedInnerChanges = pushInners @avg outer inner smoothedOuterChanges
      -- Apply the changes first so that detecting intersections is easier
      appliedOuter = outer // (applyChange outer <$> V.toList smoothedOuterChanges)
      appliedInner = inner // (applyChange inner <$> smoothedInnerChanges)
   in case linesIntersection . V.toList $ toCircularLines appliedOuter ++ toCircularLines appliedInner of
        Just p ->
          ( newgen'',
            surfOutof outer inner
          )
        Nothing ->
          ( newgen'',
            surfOutof appliedOuter appliedInner
          )
