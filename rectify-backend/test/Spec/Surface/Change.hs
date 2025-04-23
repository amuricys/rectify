{-# LANGUAGE TemplateHaskell #-}

module Spec.Surface.Change where

import Prelude

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Surface.Change
import Data.Maybe (isNothing)
import qualified Debug.Trace as Debug
import Surface.LinAlg

prop_smooth :: Property
prop_smooth = property $ do
    x <- forAll $ Gen.double (Range.linearFracFrom 0 (-100) 100)
    y <- forAll $ Gen.double (Range.linearFracFrom 0 (-100) 100)
    assert $ True -- TODO: write property tests

prop_norm :: Property
prop_norm = property $ do
    x <- forAll $ Gen.double (Range.linearFracFrom 0 (-100) 100)
    y <- forAll $ Gen.double (Range.linearFracFrom 0 (-100) 100)
    let p = Point2D (X x) (Y y)
    if x == 0 && y == 0 then
      assert $ norm p == 0
    else do
      assert $ norm p > 0.0 -- norm is always positive
      l <- forAll $ Gen.double (Range.linearFracFrom 0 (-100) 100)
      assert $ norm (Point2D (X $ x * l) (Y $ y * l)) ~== abs l * norm p -- scalar multiplication
      x' <- forAll $ Gen.double (Range.linearFracFrom 0 (-100) 100)
      y' <- forAll $ Gen.double (Range.linearFracFrom 0 (-100) 100)
      let p' = Point2D (X x') (Y y')
      diff p triangleInequality p'
        where triangleInequality p p' = norm (p `add` p') <= norm p + norm p'

tests :: Group
tests = $$discover
