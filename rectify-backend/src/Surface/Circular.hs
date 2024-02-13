{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE UndecidableInstances #-}

module Surface.Circular where

import Prelude

import Data.Aeson (FromJSON, ToJSON)
import Data.Finite (Finite, getFinite)
import Data.Proxy (Proxy (..))
import Data.Vector.Sized (Vector, generate, index)
import Data.Vector.Sized qualified as V
import GHC.TypeLits (KnownNat, natVal)
import System.Random (Random)

import Surface.Index (next, prev)
import Surface.LinAlg (Point2D (..), X (..), Y (..))

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
