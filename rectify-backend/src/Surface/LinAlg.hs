{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Surface.LinAlg where

import Prelude

import Control.Lens.TH
import Data.Aeson
import Data.Foldable
import Data.Functor.Contravariant (Contravariant (contramap), Op (..))
import Data.Kind (Type)
import Data.List (cycle, tail, tails)
import Data.Map qualified as Map
import Data.Maybe (fromJust)
import Data.Vector.Sized (Vector, cons, drop, generate, index, (++), (//))
import Data.Vector.Sized qualified as V
import GHC.Generics
import GHC.TypeLits(KnownNat, Nat, type (+))
import SimulatedAnnealing (Probability (..))
import System.Random (Random, StdGen, randomIO, randomRIO, uniformR)
import System.Random.SplitMix (SMGen, mkSMGen, nextDouble, nextInteger)
import System.Random.Stateful (StatefulGen)
import qualified Surface.Index as Index

newtype X = X {unX :: Double}
  deriving newtype (Show, Eq, Ord, Num, Fractional, FromJSON, ToJSON, Random)

newtype Y = Y {unY :: Double}
  deriving newtype (Show, Eq, Ord, Num, Fractional, FromJSON, ToJSON, Random)

data Point2D = Point2D
  { x :: X,
    y :: Y
  }
  deriving (Generic, Show, Eq)

norm :: Point2D -> Double
norm (Point2D (X x) (Y y)) = sqrt $ x ^ 2 + y ^ 2

instance FromJSON Point2D

instance ToJSON Point2D

fromTuple :: (Double, Double) -> Point2D
fromTuple (x, y) = Point2D (X x) (Y y)

fromListRepeated :: [Double] -> [Point2D]
fromListRepeated = map (\i -> Point2D (X i) (Y i))

fromListEveryTwo :: [Double] -> [Point2D]
fromListEveryTwo l = zipWith (\x y -> Point2D (X x) (Y y)) l (tail . tail $ l)

add :: Point2D -> Point2D -> Point2D
add (Point2D (X x1) (Y y1)) (Point2D (X x2) (Y y2)) = Point2D (X $ x1 + x2) (Y $ y1 + y2)

sub :: Point2D -> Point2D -> Point2D
sub (Point2D (X x1) (Y y1)) (Point2D (X x2) (Y y2)) = Point2D (X $ x1 - x2) (Y $ y1 - y2)

class ApproxEq a where
  (~==) :: a -> a -> Bool
  infix 4 ~==

-- Would love to simply say Point2Ds are Groups
instance ApproxEq Point2D where
  a ~== b = norm (a `sub` b) < 1e-6

instance ApproxEq Double where
  a ~== b = abs (a - b) < 1e-6

pwiseMult :: Point2D -> Point2D -> Point2D
pwiseMult (Point2D (X x1) (Y y1)) (Point2D (X x2) (Y y2)) = Point2D (X $ x1 * x2) (Y $ y1 * y2)

scalarMult :: Point2D -> Double -> Point2D
scalarMult (Point2D (X x) (Y y)) s = Point2D (X $ s * x) (Y $ s * y)

normed :: Point2D -> Point2D
normed p@(Point2D (X x) (Y y)) = Point2D (X $ x / norm p) (Y $ y / norm p)

dist :: Point2D -> Point2D -> Double
dist p1 p2 = norm $ p1 `sub` p2

crossProduct :: Point2D -> Point2D -> Double
crossProduct (Point2D (X x1) (Y y1)) (Point2D (X x2) (Y y2)) = x1 * y2 - y1 * x2
(×) :: Point2D -> Point2D -> Double
(×) = crossProduct
infixl 8 ×

type Line = (Point2D, Point2D)

intersection :: Line -> Line -> Maybe Point2D
intersection (p1, p2) (p3, p4) =
  let r = p2 `sub` p1
      s = p4 `sub` p3
      crossRs = crossProduct r s
   in if crossRs == 0.0 -- Collinear
        then Nothing
        else
          let t = crossProduct (p3 `sub` p1) s / crossRs
              u = crossProduct (p3 `sub` p1) r / crossRs
           in if t > 0.0 && t < 1.0 && u > 0.0 && u < 1.0
                then Just (p1 `add` r `scalarMult` t)
                else Nothing

linesIntersection :: [Line] -> Maybe Point2D
linesIntersection [] = Nothing
linesIntersection [_] = Nothing
linesIntersection l =
  let -- Each list in ts must check for intersections between its head and the rest of the list
      ts :: [[Line]]
      ts = l : tails l
   in foldl' go Nothing ts
  where
    go :: Maybe Point2D -> [Line] -> Maybe Point2D
    go (Just p) _ = Just p
    go x [] = x
    go x [_] = x
    go Nothing (l1 : l2 : ls) = case intersection l1 l2 of
      -- Recurse on the head because ts is a list of tails
      Nothing -> go Nothing (l1 : ls)
      Just p -> Just p
