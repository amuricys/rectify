{-# LANGUAGE TemplateHaskell #-}

module Spec.LinAlg where
import Prelude

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Surface.LinAlg
import Data.Maybe (isNothing)
import qualified Debug.Trace as Debug

prop_norm :: Property
prop_norm = property $ do
    x <- forAll $ Gen.double (Range.linearFracFrom 0 (-100) 100)
    y <- forAll $ Gen.double (Range.linearFracFrom 0 (-100) 100)
    let p = Point2D (X x) (Y y)
    if x == 0 && y == 0 then
      assert $ norm p == 0
    else do
      assert $ norm p > 0 -- norm is always positive
      l <- forAll $ Gen.double (Range.linearFracFrom 0 (-100) 100)
      assert $ norm (Point2D (X $ x * l) (Y $ y * l)) ~== abs l * norm p -- scalar multiplication
      x' <- forAll $ Gen.double (Range.linearFracFrom 0 (-100) 100)
      y' <- forAll $ Gen.double (Range.linearFracFrom 0 (-100) 100)
      let p' = Point2D (X x') (Y y')
      diff p fn p'
        where fn p p' = norm (p `add` p') <= norm p + norm p'  -- triangle inequality

prop_normed :: Property
prop_normed = property $ do
    x <- forAll $ Gen.double (Range.linearFracFrom 0 (-100) 100)
    y <- forAll $ Gen.double (Range.linearFracFrom 0 (-100) 100)
    let p = Point2D (X x) (Y y)
    if x == 0.0 && y == 0 then
      assert . isNaN . norm $ normed p
    else do
      diff p (\p val -> norm (normed p) ~== val) 1.0 

prop_intersection :: Property
prop_intersection = property $ do
    x1 <- forAll $ Gen.double (Range.linearFracFrom 0 (-100) 100)
    y1 <- forAll $ Gen.double (Range.linearFracFrom 0 (-100) 100)
    x2 <- forAll $ Gen.double (Range.linearFracFrom 0 (-100) 100)
    y2 <- forAll $ Gen.double (Range.linearFracFrom 0 (-100) 100)
    x3 <- forAll $ Gen.double (Range.linearFracFrom 0 (-100) 100)
    y3 <- forAll $ Gen.double (Range.linearFracFrom 0 (-100) 100)
    x4 <- forAll $ Gen.double (Range.linearFracFrom 0 (-100) 100)
    y4 <- forAll $ Gen.double (Range.linearFracFrom 0 (-100) 100)
    let p1 = Point2D (X x1) (Y y1)
        p2 = Point2D (X x2) (Y y2)
        p3 = Point2D (X x3) (Y y3)
        p4 = Point2D (X x4) (Y y4)
    case intersection (p1, p2) (p3, p4) of
      Nothing ->   -- if there's no intersection between these random points,
        success    -- we can't say anything, but
      Just p -> do -- if there is, such an intersection must be found by linesIntersection
        let lines = [(p1, p2), (p2, p3), (p3, p4), (p4, p1)]
        annotateShow lines
        diff p fn $ linesIntersection lines
          where
            fn :: Point2D -> Maybe Point2D -> Bool
            fn p'  = maybe False (~== p')

prop_crossProduct :: Property
prop_crossProduct = property $ do
    x1 <- forAll $ Gen.double (Range.linearFracFrom 0 (-100) 100)
    y1 <- forAll $ Gen.double (Range.linearFracFrom 0 (-100) 100)
    x2 <- forAll $ Gen.double (Range.linearFracFrom 0 (-100) 100)
    y2 <- forAll $ Gen.double (Range.linearFracFrom 0 (-100) 100)
    x3 <- forAll $ Gen.double (Range.linearFracFrom 0 (-100) 100)
    y3 <- forAll $ Gen.double (Range.linearFracFrom 0 (-100) 100)
    let p1 = Point2D (X x1) (Y y1)
        p2 = Point2D (X x2) (Y y2)
        p3 = Point2D (X x3) (Y y3)
    assert $ p1 × p2 ~== p2 × p1 * (-1) -- antisymmetry
    assert $ p1 × (p2 `add` p3) ~== (p1 × p2) + (p1 × p3) -- distributivity
    assert $ p1 × (p2 `scalarMult` 2) ~== (p1 × p2) * 2 -- scalar multiplication


tests :: Group
tests = $$discover
