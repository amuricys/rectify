{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE UndecidableInstances #-}

module Surface where

import Data.Aeson (FromJSON, ToJSON (toJSON), Value)
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
import GHC.Generics (Generic)
import GHC.TypeLits (CmpNat, KnownNat, Mod, Nat, OrderingI (LTI), SomeNat (SomeNat), cmpNat, natVal, someNatVal, type (+), type (-), type (<=?))
import SimulatedAnnealing (Probability (..), Problem (..))
import Surface.Circular (Circular, Compression (Compression), Radius (..), circularGraph)
import Surface.Index qualified as Index
import Surface.LinAlg (Point2D (Point2D), X (X), Y (Y), dist, linesIntersection, toLines)
import System.Random.SplitMix (SMGen, nextDouble, nextInteger)
import Prelude hiding (drop, (++))
import Debug.Pretty.Simple

-- Hmm.
class SurfaceData (dim :: Nat) where
  type Point dim :: Type
  volume :: forall o i. Surface o i (Point dim) (Point dim) -> Double
  perimeter :: forall o i. Surface o i (Point dim) (Point dim) -> Double

data Surface o i outerPoint innerPoint = Surface
  { outer :: Vector o outerPoint,
    inner :: Vector i innerPoint
  }
  deriving (Generic, Show, Eq)

instance ToJSON a => ToJSON (Vector n a) where
  toJSON :: ToJSON a => Vector n a -> Value
  toJSON = toJSON . V.toList

instance (ToJSON op, ToJSON ip) => ToJSON (Surface o i op ip)

-- addOuterPoint :: forall n m point i. (KnownNat n) => point -> Surface (n + m) i point -> Surface (n + m) i point
addOuterPoint :: forall n m i op ip. (KnownNat n) => op -> Surface (n + m) i op ip -> Surface (n + (1 + m)) i op ip
addOuterPoint p (Surface outer inner) = Surface (V.take @n outer ++ cons p (drop @n outer)) inner

addInnerPoint :: forall n m o op ip. (KnownNat n) => ip -> Surface o (n + m) op ip -> Surface o (n + (1 + m)) op ip
addInnerPoint p (Surface outer inner) = Surface outer (V.take @n inner ++ cons p (drop @n inner))

data Change = Change
  { xChange :: X,
    yChange :: Y
  }
  deriving (Generic, Show, Eq)

scaleChange :: Double -> Change -> Change
scaleChange s (Change x y) = Change (X s * x) (Y s * y)

instance Semigroup Change where
  Change x y <> Change x' y' = Change (x + x') (y + y')

instance Monoid Change where
  mempty = Change 0 0

newtype Thickness = Thickness {unThickness :: Double}
  deriving newtype (Show, Eq, Num, Fractional, FromJSON, ToJSON)

circularSurface2D :: forall o i. (KnownNat o) => (KnownNat i) => X -> Y -> Radius -> Thickness -> Surface o i Point2D Point2D
circularSurface2D centerx centery radius thickness =
  Surface
    (circularGraph centerx centery radius)
    (circularGraph centerx centery (Radius $ unRadius radius - unThickness thickness))

smooth ::
  forall range n.
  (KnownNat n, KnownNat range) =>
  -- | The current change's index in its surface
  Finite n ->
  -- | The current change's value
  Change ->
  -- | A function that describes how changes scale
  (Integer -> Integer -> Change -> Change) ->
  Vector (range + 1 + range) (Finite n, Change)
smooth i c fn = V.reverse backward ++ V.singleton (i, c) ++ forward
  where
    range = natVal (Proxy @range)
    forward, backward :: Vector range (Finite n, Change)
    forward = V.generate $ \ind -> (Index.add i (getFinite ind + 1), fn (getFinite ind) range c)
    backward = V.generate $ \ind -> (Index.sub i (getFinite ind + 1), fn (getFinite ind) range c)

linear :: Integer -> Integer -> Change -> Change
linear at max Change {xChange, yChange} = Change (xChange * X ratio) (yChange * Y ratio)
  where
    ratio = fromIntegral (max - at) / fromIntegral (max + 1)

change :: Point2D -> Change -> Point2D
change (Point2D x y) (Change x' y') = Point2D (x + x') (y + y')

newtype Stitching i = Stitching {unStitching :: Map.Map (Finite i) (Unsized.Vector (Finite i, Double, Double))}
  deriving newtype (Show, Eq)

avgChangeForClosest :: forall n m o i. KnownNat n => Point2D -> Vector m (Finite o, Change) -> Circular o -> Change
avgChangeForClosest p outerChanges g =
  let changesAsList = V.toList outerChanges
      sorted = snd <$> take (fromIntegral $ natVal (Proxy @n)) (sortBy sorter changesAsList)
   in avg . fromJust $ V.fromList @n sorted -- TODO: fromJust
  where
    sorter (i1, c1) (i2, c2) = compare (dist (g `index` i1) p) (dist (g `index` i2) p)
    avg = scaleChange (1 / fromIntegral (natVal (Proxy @n))) . V.foldl' (<>) mempty

innerChangesFromOuterChanges :: forall avg i o n m. KnownNat avg => [Finite i] -> Vector m (Finite o, Change) -> Circular i -> Circular o -> [(Finite i, Change)]
innerChangesFromOuterChanges internalIndices outerChanges inner outer =
  flip fmap internalIndices \i ->
    let avgChange = avgChangeForClosest @avg (inner `index` i) outerChanges outer
     in (i, avgChange)

-- Takes in a circular graph and a map of changes and finds two nodes: the two that are OUTSIDE the map
-- of changes but IN in the graph whose prev and next ARE in the map of changes
mostPrevNext :: forall m mprev n. m ~ mprev + 1 => m ~ 1 + mprev =>  KnownNat n => Vector m (Finite n, Change) -> Circular n -> (Point2D, Point2D)
mostPrevNext nc g =
  let mostNext = Index.next . fst  . V.last $ nc
      mostPrev = Index.prev . fst  . V.head $ nc
   in (g `index` mostPrev, g `index` mostNext)

-- TODO: Partition-optimize
closestInternalNodes :: forall m n nprev. KnownNat n => n ~ nprev + 1 => Point2D -> Point2D -> Circular n -> (LessThan n, LessThan n)
closestInternalNodes mostPrev mostNext inner = (closestNodeTo mostPrev, closestNodeTo mostNext)
  where
    closestNodeTo :: Point2D -> LessThan n
    closestNodeTo p = lessThan $ V.minIndexBy (\p1 p2 -> compare (dist p1 p) (dist p2 p)) inner

data LessThan i where
  LessThan :: forall n m i. (KnownNat n, CmpNat n i ~ LT) => Proxy n -> LessThan i

lessThan :: forall n. KnownNat n => Finite n -> LessThan n
lessThan i = case someNatVal (getFinite i) of
  Just (SomeNat (_ :: Proxy i)) -> case cmpNat (Proxy @i) (Proxy @n) of
    LTI -> LessThan @i @n Proxy
    _ -> error "Finite contained integer larger than its bound"
  Nothing -> error "Finite contained negative integer"

type family Dist (n :: Nat) (m :: Nat) (modulo :: Nat) :: Nat where
  Dist n m modulo = If (n <=? m) (m - n) (Mod (n + m + 1) modulo)

pushInners ::
  forall avg o i n nprev oprev iprev.
  KnownNat o =>
  KnownNat i =>
  KnownNat avg =>
  avg < i =>
  o ~ oprev + 1 =>
  i ~ iprev + 1 =>
  n ~ nprev + 1 =>
  n ~ 1 + nprev =>
  Surface o i Point2D Point2D ->
  Vector n (Finite o, Change) ->
  Compression ->
  [(Finite i, Change)]
pushInners (Surface outer inner) outerChanges compression =
  let (mostPrev, mostNext) = mostPrevNext outerChanges outer
   in case closestInternalNodes mostPrev mostNext inner of
        (LessThan (p1 :: Proxy k), LessThan (p2 :: Proxy m)) ->
          let internalIndices :: [Finite i]
              internalIndices = takeWhile (/= finite (natVal p2)) $ iterate Index.next (finite $ natVal p1)
           in innerChangesFromOuterChanges @avg internalIndices outerChanges inner outer

modifySurf ::
  forall avg o i oprev iprev.
  KnownNat o =>
  o ~ oprev + 1 =>
  KnownNat i =>
  i ~ iprev + 1 =>
  KnownNat avg =>
  avg < i =>
  (Double, Double) -> -- range of random change to x and y
  SMGen ->
  Surface o i Point2D Point2D ->
  (SMGen, Surface o i Point2D Point2D)
modifySurf (high, low) gen s@(Surface outer inner) =
  let -- Choose a random point on the outer surface (- 1 because nextInteger interval is closed)
      (i, newgen) = first finite $ nextInteger 0 (natVal (Proxy @o) - 1) gen
      -- Choose a random change to x and y
      (xChange, newgen') = first (\x -> X $ x * 2 * high + low) (nextDouble newgen)
      (yChange, newgen'') = first (\y -> Y $ y * 2 * high + low) (nextDouble newgen')
      c = Change xChange yChange
      smoothedOuterChanges = smooth @8 i c linear
      smoothedInnerChanges = pushInners @avg s smoothedOuterChanges (Compression 0.0)
      -- Apply the changes first so that detecting intersections is easier
      appliedOuter = outer // (applyChange outer <$> V.toList smoothedOuterChanges)
      appliedInner = inner // (applyChange inner <$> smoothedInnerChanges)
      possibly = toLines (V.toList appliedOuter) <> toLines (V.toList appliedInner)
   in case linesIntersection possibly of
        Just p -> (newgen'', s)
        Nothing -> (newgen'', Surface appliedOuter appliedInner)
  where
    applyChange :: forall n. Vector n Point2D -> (Finite n, Change) -> (Finite n, Point2D)
    applyChange g (i, c) = (i, change (g `index` i) c)
