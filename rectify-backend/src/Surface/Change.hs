{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module Surface.Change where

import Prelude
import Surface.LinAlg (X(..), Y(..), Point2D (Point2D), dist)
import GHC.Generics (Generic)
import qualified Data.Vector.Sized as V
import GHC.TypeLits (KnownNat, type (+), natVal, CmpNat, SomeNat (SomeNat), OrderingI (LTI), cmpNat, someNatVal)
import Data.Vector.Sized (Vector)
import Surface.Index (Index)
import Surface.Circular (Circular)
import qualified Surface.Index as Index
import Data.Proxy (Proxy(..))
import Data.Finite (getFinite, finite)
import Data.List (sortBy)

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

-- | One of the functions that may be given to `smooth`
linear :: Integer -> Integer -> Change -> Change
linear at max Change {xChange, yChange} = Change (xChange * X ratio) (yChange * Y ratio)
  where
    ratio = fromIntegral (max - at) / fromIntegral (max + 1)

-- | A function that given an index and a change, produces a
-- a vector of changes that smoothly transitions from the given change
smooth ::
  forall range n.
  (KnownNat n, KnownNat range) =>
  -- | The current change's index in its surface
  Index n ->
  -- | The current change's value
  Change ->
  -- | A function that describes how changes scale
  (Integer -> Integer -> Change -> Change) ->
  Vector (range + 1 + range) (Index n, Change)
smooth i c fn = V.reverse backward V.++ V.singleton (i, c) V.++ forward
  where
    range = natVal (Proxy @range)
    forward, backward :: Vector range (Index n, Change)
    forward = V.generate $ \ind -> (Index.add i (getFinite ind + 1), fn (getFinite ind) range c)
    backward = V.generate $ \ind -> (Index.sub i (getFinite ind + 1), fn (getFinite ind) range c)


hmm :: Vector 11 (Index 20, Change)
hmm = smooth @5 @20 5 Change {xChange = 0.0, yChange = 10.0} linear

-- newtype Stitching i = Stitching {unStitching :: Map.Map (Index i) (Unsized.Vector (Index i, Double, Double))}
--   deriving newtype (Show, Eq)


-- | Given a point, a vector of changes paired with their target vertices and a graph, 
-- this function returns a change that is the average of the `n` changes whose
-- vertex targets are closest to the point.
avgChangeForClosest :: forall n mostCtr mostClk m o i. 
  KnownNat n =>
  KnownNat i =>
  KnownNat mostCtr =>
  KnownNat mostClk =>
  Index i ->
  Point2D ->
  Vector m (Index o, Change) -> Circular o -> Change
avgChangeForClosest i p outerChanges g =
  let changesAsList = V.toList outerChanges
      sorted = snd <$> take (fromIntegral avgOf) (sortBy sorter changesAsList)
   in avg sorted
  where
    sorter (i1, _) (i2, _) = compare (dist (g `V.index` i1) p) (dist (g `V.index` i2) p)
    avgOf = minimum [natVal (Proxy @n),
                     Index.distClkwise @mostClk i,
                     Index.distCtrclkwise @mostCtr i]
    avg :: [Change] -> Change
    avg = scaleChange (1 / fromIntegral avgOf) . foldl (<>) mempty

data LessThan i where
  LessThan :: forall n m i. (KnownNat n, CmpNat n i ~ LT) => Proxy n -> LessThan i

lessThan :: forall n. KnownNat n => Index n -> LessThan n
lessThan i = case someNatVal (getFinite i) of
  Just (SomeNat (_ :: Proxy i)) -> case cmpNat (Proxy @i) (Proxy @n) of
    LTI -> LessThan @i @n Proxy
    _ -> error "Index contained integer larger than its bound"
  Nothing -> error "Index contained negative integer"

-- TODO: Partition-optimize.
-- | Given two points and a circular graph, this function returns the indices of the two nodes
-- that are closest to the given points.
closestInternalNodes :: forall m n nprev. KnownNat n => n ~ nprev + 1 => Point2D -> Point2D -> Circular n -> (LessThan n, LessThan n)
closestInternalNodes mostCtrclkwise mostClkwise inner = (closestNodeTo mostCtrclkwise, closestNodeTo mostClkwise)
  where
    closestNodeTo :: Point2D -> LessThan n
    closestNodeTo p = lessThan $ V.minIndexBy (\p1 p2 -> compare (dist p1 p) (dist p2 p)) inner

pushInners ::
  forall avg o i n nprev oprev iprev.
  KnownNat o =>
  KnownNat i =>
  KnownNat avg =>
  o ~ oprev + 1 =>
  i ~ iprev + 1 =>
  n ~ nprev + 1 =>
  n ~ 1 + nprev =>
  Circular o ->
  Circular i ->
  Vector n (Index o, Change) ->
  [(Index i, Change)]
pushInners outer inner outerChanges =
  let mostClkwise = V.index outer . Index.next . fst . V.last $ outerChanges
      mostCtrclkwise = V.index outer . Index.prev . fst . V.head $ outerChanges
   in case closestInternalNodes mostCtrclkwise mostClkwise inner of
        (LessThan (p1 :: Proxy mostCtr), LessThan (p2 :: Proxy mostClk)) ->
          let indexAndChange :: Index i -> (Index i, Change)
              indexAndChange i = (i, avgChangeForClosest @avg @mostCtr @mostClk i (inner `V.index` i) outerChanges outer)
              internalIndices :: [Index i]
              internalIndices = takeWhile (/= finite (natVal p2)) $ iterate Index.next (finite $ natVal p1)
           in fmap indexAndChange internalIndices

applyChange :: forall n. Vector n Point2D -> (Index n, Change) -> (Index n, Point2D)
applyChange g (i, c) = (i, change (g `V.index` i) c)
  where change :: Point2D -> Change -> Point2D
        change (Point2D x y) (Change x' y') = Point2D (x + x') (y + y')
