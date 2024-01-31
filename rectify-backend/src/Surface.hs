{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE UndecidableInstances #-}

module Surface where

import RIO hiding (Vector, drop, take, (++), ask, Reader)
import Control.Lens.TH
import Data.Finite
import Data.Vector.Sized
import Effectful
import Effectful.Dispatch.Dynamic (interpret, reinterpret)
import Effectful.Reader.Static
import Effectful.State.Static.Local
import GHC.TypeNats
import System.Random (Random, StdGen, randomIO, randomRIO, uniformR)
import System.Random.Stateful (StatefulGen)

data Point2D = Point2D
  { id :: Integer,
    x :: X,
    y :: Y
  }
  deriving (Generic, Show, Eq)

data Surface o i point = Surface
  { outer :: Vector o point,
    inner :: Vector i point
  }
  deriving (Generic, Show, Eq)

newtype Radius = Radius {unRadius :: Double}
  deriving newtype (Show, Eq, Num, Fractional)

newtype Thickness = Thickness {unThickness :: Double}
  deriving newtype (Show, Eq, Num, Fractional)

newtype X = X {unX :: Double}
  deriving newtype (Show, Eq, Num, Fractional, Random)

newtype Y = Y {unY :: Double}
  deriving newtype (Show, Eq, Num, Fractional, Random)

-- addOuterPoint :: forall n m point i. (KnownNat n) => point -> Surface (n + m) i point -> Surface (n + m) i point
addOuterPoint :: forall n m i point. (KnownNat n) => point -> Surface (n + m) i point -> Surface (n + (1 + m)) i point
addOuterPoint p (Surface outer inner) = Surface (take @n outer ++ cons p (drop @n outer)) inner

addInnerPoint :: forall n m o point. (KnownNat n) => point -> Surface o (n + m) point -> Surface o (n + (1 + m)) point
addInnerPoint p (Surface outer inner) = Surface outer (take @n inner ++ cons p (drop @n inner))

surface2DRandom :: forall n. (KnownNat n) => IO (Surface n n Point2D)
surface2DRandom = Surface <$> generateM @n initPoint <*> generateM @n initPoint
  where
    initPoint i = Point2D (getFinite i) <$> (subtract 1.0 <$> randomIO) <*> (subtract 1.0 <$> randomIO)

circularGraph :: forall n. (KnownNat n) => X -> Y -> Radius -> Vector n Point2D
circularGraph (X centerx) (Y centery) (Radius radius) =
  generate $ \i -> Point2D (getFinite i) (x i) (y i)
  where
    x i = X (centerx + radius * cos (2 * pi * fromIntegral (getFinite i) / fromIntegral (natVal (Proxy :: Proxy n))))
    y i = Y (centery + radius * sin (2 * pi * fromIntegral (getFinite i) / fromIntegral (natVal (Proxy :: Proxy n))))

circularSurface2D :: forall o i. (KnownNat o) => (KnownNat i) => X -> Y -> Radius -> Thickness -> Surface o i Point2D
circularSurface2D centerx centery radius thickness =
  Surface
    (circularGraph centerx centery radius)
    (circularGraph centerx centery (Radius $ unRadius radius - unThickness thickness))

data SimState s =
  SimState
  { surface :: s
  , temperature :: Double }
  deriving (Generic, Show)

data SimParams = SimParams
  { nodeAdditionThreshold :: Double,
    nodeDeletionThreshold :: Double,
    howSmooth :: Double,
    thickness :: Double
  }
  deriving (Show, Eq)

temperatureFunction = undefined

neighbor :: m (Surface o i Point2D)
neighbor = do
  undefined

energy :: Surface o i p -> Double
energy = undefined

-- step :: forall o i. Eff '[State (Surface o i Point2D), Reader SimParams, Rand] ()
-- step = do
--   temp <- gets (^. temperature)
--   n <- neighbor
--   SimState {surface} <- get
--   let e1 = energy _surface
--       e2 = energy n
--   modify $ \s -> s & temperature .~ temperatureFunction temp


-- randomNumber :: IO Double
-- randomNumber = runEff . runSimulation $ randomR (0.0, 1.0)

-- >>> randomNumber
-- Variable not in scope: randomNumber
