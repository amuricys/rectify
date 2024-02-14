{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE UndecidableInstances #-}

module Surface.Circular where

import Prelude hiding ((++))

import Data.Aeson (FromJSON, ToJSON)
import Data.Finite (Finite, getFinite)
import Data.Proxy (Proxy (..))
import Data.Vector.Sized (Vector, generate, index, (++))
import Data.Vector.Sized qualified as V
import GHC.TypeLits (KnownNat, natVal, type (+))
import Surface.Index (next, prev)
import Surface.Index qualified as Index
import Surface.LinAlg (Line, Point2D (..), X (..), Y (..), add, dist, scalarMult)
import System.Random (Random)
import Unsafe.Coerce (unsafeCoerce)

type Circular i = Vector i Point2D

newtype Radius = Radius {unRadius :: Double}
  deriving newtype (Show, Eq, Num, Fractional, FromJSON, ToJSON)

newtype Compression = Compression {unCompression :: Double}
  deriving newtype (Show, Eq, Num, Ord, Fractional, FromJSON, ToJSON, Random)

circularGraph :: forall n. (KnownNat n) => X -> Y -> Radius -> Circular n
circularGraph (X centerx) (Y centery) (Radius radius) =
  generate $ \i -> Point2D (xCoord i) (yCoord i)
  where
    xCoord i = X (centerx + radius * cos (2 * pi * fromIntegral (getFinite i) / fromIntegral (natVal (Proxy :: Proxy n))))
    yCoord i = Y (centery + radius * sin (2 * pi * fromIntegral (getFinite i) / fromIntegral (natVal (Proxy :: Proxy n))))

area :: forall n. KnownNat n => Circular n -> Double
area v = V.foldl' (\acc i -> acc + unX (x (v `index` i)) * unY (y (v `index` next i) - y (v `index` prev i))) 0.0 indices
  where
    indices :: Vector n (Finite n)
    indices = generate id

addPoint :: forall i n m. (KnownNat n, KnownNat i, n ~ i + m) => Point2D -> Circular n -> Circular (1 + n)
addPoint p g = convert $ V.take @i g ++ V.cons p (V.drop @i g)
  where
    convert :: Circular (i + (1 + m)) -> Circular (1 + n)
    convert = unsafeCoerce

toCircularLines :: KnownNat n => Vector n Point2D -> Vector n Line
toCircularLines ps = generate $ \i -> (ps `index` i, ps `index` Index.next i)

toAdd :: forall n. KnownNat n => Double -> Vector n Line -> [(Finite n, Point2D)]
toAdd threshold v = V.foldl' folder [] (V.generate id)
  where
    folder :: [(Finite n, Point2D)] -> Finite n -> [(Finite n, Point2D)]
    folder acc i =
      if dist p1 p2 > threshold
        then (i, p1 `add` p2 `scalarMult` 0.5) : acc
        else acc
      where
        (p1, p2) = v `index` i

