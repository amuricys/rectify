-- This is a comparison kernel, not a complete polygon validity predicate.
module Rectify.Geometry.Segments
  ( Point, Segment, properIntersection, countProperIntersections, signedTwiceArea
  ) where

import Data.List (tails)

type Point = (Double, Double)
type Segment = (Point, Point)

sub :: Point -> Point -> Point
sub (x, y) (a, b) = (x - a, y - b)

cross :: Point -> Point -> Double
cross (x, y) (a, b) = x * b - y * a

-- Mirrors the old surface predicate's strict interior-crossing convention.
-- Collinear overlaps and endpoint contacts are excluded deliberately.
-- Exact comparison to zero is retained; no robustness claim is made.
properIntersection :: Segment -> Segment -> Bool
properIntersection (p, q) (a, b)
  | denominator == 0 = False
  | otherwise = t > 0 && t < 1 && u > 0 && u < 1
  where
    r = sub q p
    s = sub b a
    denominator = cross r s
    t = cross (sub a p) s / denominator
    u = cross (sub a p) r / denominator

-- Enumerates each unordered pair once. This is the quadratic baseline for
-- a future spatial-partition experiment, not an optimized implementation.
countProperIntersections :: [Segment] -> Int
countProperIntersections segments =
  length [() | first : rest <- tails segments, other <- rest,
               properIntersection first other]

-- Closing edge is included. Orientation changes the sign.
signedTwiceArea :: [Point] -> Double
signedTwiceArea [] = 0
signedTwiceArea points@(first : rest) =
  sum [cross a b | (a, b) <- zip points (rest ++ [first])]
