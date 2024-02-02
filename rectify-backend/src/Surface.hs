{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Surface where

import Control.Lens.TH
import Data.Aeson
import Data.Finite
import Data.Kind (Type)
import Data.Vector.Sized
import Data.Vector.Sized qualified as V
import Effectful
import Effectful.Dispatch.Dynamic (interpret, reinterpret)
import Effectful.Reader.Static
import Effectful.State.Static.Local
import GHC.TypeLits (natVal)
import GHC.TypeNats (KnownNat, Nat, type (+))
import RIO hiding (Reader, Vector, ask, drop, take, (++))
import SimulatedAnnealing (Probability (..))
import System.Random (Random, StdGen, randomIO, randomRIO, uniformR)
import System.Random.SplitMix (SMGen, nextDouble, nextInteger, mkSMGen)
import System.Random.Stateful (StatefulGen)

-- Hmm.
class SurfaceData (dim :: Nat) where
  type Point dim :: Type
  volume :: forall o i. Surface o i (Point dim) (Point dim) -> Double
  perimeter :: forall o i. Surface o i (Point dim) (Point dim) -> Double

instance (KnownNat n) => FromJSON (Finite n) where
  parseJSON = fmap finite . parseJSON
instance ToJSON (Finite n) where
  toJSON = toJSON . getFinite

data Point2D i = Point2D
  { id :: Finite i,
    x :: X,
    y :: Y
  }
  deriving (Generic, Show, Eq)
instance KnownNat i => FromJSON (Point2D i)
instance ToJSON (Point2D i)

data Surface o i outerPoint innerPoint = Surface
  { outer :: Vector o outerPoint,
    inner :: Vector i innerPoint
  }
  deriving (Generic, Show, Eq)
instance ToJSON a => ToJSON (Vector n a) where
  toJSON :: ToJSON a => Vector n a -> Value
  toJSON = toJSON . V.toList
instance (ToJSON op, ToJSON ip) => ToJSON (Surface o i op ip)

newtype Radius = Radius {unRadius :: Double}
  deriving newtype (Show, Eq, Num, Fractional, FromJSON, ToJSON)

newtype Thickness = Thickness {unThickness :: Double}
  deriving newtype (Show, Eq, Num, Fractional, FromJSON, ToJSON)

newtype X = X {unX :: Double}
  deriving newtype (Show, Eq, Num, Fractional, FromJSON, ToJSON, Random)

newtype Y = Y {unY :: Double}
  deriving newtype (Show, Eq, Num, Fractional, FromJSON, ToJSON, Random)

newtype Energy = Energy {unEnergy :: Double}
  deriving newtype (Show, Eq, Num, Ord, Fractional, FromJSON, ToJSON, Random)

newtype Temperature = Temperature {unTemperature :: Double}
  deriving newtype (Show, Eq, Num, Ord, Fractional, FromJSON, ToJSON, Random)

-- addOuterPoint :: forall n m point i. (KnownNat n) => point -> Surface (n + m) i point -> Surface (n + m) i point
addOuterPoint :: forall n m i op ip. (KnownNat n) => op -> Surface (n + m) i op ip -> Surface (n + (1 + m)) i op ip
addOuterPoint p (Surface outer inner) = Surface (take @n outer ++ cons p (drop @n outer)) inner

addInnerPoint :: forall n m o op ip. (KnownNat n) => ip -> Surface o (n + m) op ip -> Surface o (n + (1 + m)) op ip
addInnerPoint p (Surface outer inner) = Surface outer (take @n inner ++ cons p (drop @n inner))

circularGraph :: forall n. (KnownNat n) => X -> Y -> Radius -> Vector n (Point2D n)
circularGraph (X centerx) (Y centery) (Radius radius) =
  generate $ \i -> Point2D i (xCoord i) (yCoord i)
  where
    xCoord i = X (centerx + radius * cos (2 * pi * fromIntegral (getFinite i) / fromIntegral (natVal (Proxy :: Proxy n))))
    yCoord i = Y (centery + radius * sin (2 * pi * fromIntegral (getFinite i) / fromIntegral (natVal (Proxy :: Proxy n))))

circularSurface2D :: forall o i. (KnownNat o) => (KnownNat i) => X -> Y -> Radius -> Thickness -> Surface o i (Point2D o) (Point2D i)
circularSurface2D centerx centery radius thickness =
  Surface
    (circularGraph centerx centery radius)
    (circularGraph centerx centery (Radius $ unRadius radius - unThickness thickness))

sched :: Integer -> Temperature
sched i =
  let progression = 1.0 - fromIntegral i / 200.0
   in if progression < 0.0
        then 0.0
        else Temperature progression

next :: forall n. KnownNat n => Finite n -> Finite n
next i'
  | i' == finite (natVal (Proxy :: Proxy n) - 1) = 0
  | otherwise = i' + 1
prev :: forall n. KnownNat n => Finite n -> Finite n
prev i'
  | i' == 0 = finite $ natVal (Proxy :: Proxy n) - 1
  | otherwise = i' - 1

data Change (i :: Nat) = Change
  {
    xChange :: X,
    yChange :: Y
  }
  deriving (Generic, Show, Eq)
instance Semigroup (Change i) where
  Change x y <> Change x' y'  = Change (x + x') (y + y')
instance Monoid (Change i) where
  mempty = Change 0 0

-- smooth :: KnownNat n => Finite n -> b -> (Integer -> b -> b) -> Integer -> [(Finite n, b)]
smooth i c fn range =  [(i, c)] <> forward i 0 <> backward i 0
  where
    forward indf n
        | n == range = []
        | otherwise  = (indf, fn n range c) : forward (next indf) (n + 1)
    backward indb n
        | n == range = []
        | otherwise  = (indb, fn n range c) : backward (prev indb) (n + 1)

linear :: Integer -> Integer -> Change o -> Change o
linear at max Change {xChange, yChange} = Change (xChange * X ratio) (yChange * Y ratio)
  where
    ratio = fromIntegral at / fromIntegral max

change :: Point2D i -> Change i -> Point2D i
change (Point2D i x y) (Change x' y') = Point2D i (x + x') (y + y')

modifySurf ::
  forall o i.
  KnownNat o =>
  KnownNat i =>
  (Double, Double) -> -- range of random change to x and y
  SMGen ->
  Surface o i (Point2D o) (Point2D i) ->
  Surface o i (Point2D o) (Point2D i)
modifySurf (high, low) gen (Surface outer inner) = Surface (outer // smoothedChanges) inner
  where
    -- Choose a random point on the outer surface (- 1 because nextInteger interval is closed)
    (i, newgen) = first finite $ nextInteger 0 (natVal (Proxy @o) - 1) gen
    -- Choose a random change to x and y
    (xChange, newgen') = first (\x -> X $ x * high + low * 0.5) (nextDouble newgen)
    (yChange, newgen'') = first (\y -> Y $ y * high + low * 0.5) (nextDouble newgen')
    c = Change xChange yChange
    smoothedChanges = (\(i, c) -> (i, change (outer `index` i) c)) <$> smooth i c linear 5

acceptanceProbability :: Energy -> Energy -> Temperature -> Probability
acceptanceProbability energyState energyNeighbor temperature
  | temperature <= 0.0 = if energyNeighbor < energyState then 1.0 else 0.0
  | otherwise = Probability $ exp (unEnergy (energyState - energyNeighbor) / unTemperature temperature)

area :: forall n. KnownNat n => Vector n (Point2D n) -> Double
area v = V.foldl' (\acc i -> acc + unX (x (v `index` i)) * unY (y (v `index` next i) - y (v `index` prev i))) 0.0 indices
  where
    indices :: Vector n (Finite n)
    indices = generate RIO.id

-- >>> area (circularGraph @50 (X 0.0) (Y 0.0) (Radius 1.0))
-- 6.266661678215213

freeEnergy :: forall o i. KnownNat o => KnownNat i => Double -> Surface o i (Point2D o) (Point2D i) -> Energy
freeEnergy initialGrayMatter surf = Energy $ whiteMatter + (1.0 + grayMatterStretch) ** 2.0
  where
    whiteMatter = area (outer surf)
    grayMatter = abs $ area (inner surf) - whiteMatter
    grayMatterStretch = abs $ (grayMatter - initialGrayMatter) ** 2.0

seed :: SMGen
seed = mkSMGen 1234