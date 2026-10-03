module Reservoir.Matrix where

import Clash.Prelude

type Matrix m n a = Vec m (Vec n a)

dot ::
  KnownNat n =>
  1 <= n =>
  Num a =>
  Vec n a ->
  Vec n a ->
  a
dot vec1 vec2 =
  sum (zipWith (*) vec1 vec2)

-- | Matrix/vector multiplication
mvMult ::
  KnownNat n =>
  1 <= n =>
  Num a =>
  -- \^ Constraints needed for `dot`

  -- | Matrix with `m` rows, `n` columns
  Matrix m n a ->
  -- | Vector with `n` integers
  Vec n a ->
  Vec m a
mvMult mat vec =
  map (dot vec) mat

mmMult 
  :: an ~ bm
  => 1 <= bm
  => KnownNat bn
  => KnownNat bm
  => Num a
  => Matrix am an a
  -> Matrix bm bn a
  -> Matrix am bn a
mmMult mat1 mat2 = 
  map (mvMult (transpose mat2)) mat1
