{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Problem where

import Prelude

import Data.Aeson (FromJSON, ToJSON)
import GHC.TypeLits (KnownNat, type (+), type (<=))
import SimulatedAnnealing (Probability (Probability), Problem (..))
import Surface (Surface (inner, outer), Thickness, circularSurface2D, modifySurf)
import Surface.Circular (Radius (unRadius), area)
import Surface.LinAlg (Point2D)
import System.Random.SplitMix (SMGen, mkSMGen)
import Data.Type.Ord (type (<))

newtype Energy = Energy {unEnergy :: Double}
  deriving newtype (Show, Eq, Num, Ord, Fractional, FromJSON, ToJSON)

newtype Temperature = Temperature {unTemperature :: Double}
  deriving newtype (Show, Eq, Num, Ord, Fractional, FromJSON, ToJSON)

surfaceProblem ::
  forall o i avg oprev iprev.
  i ~ iprev + 1 =>
  o ~ oprev + 1 =>
  avg < i =>
  KnownNat o =>
  KnownNat i =>
  KnownNat avg =>
  -- | Surface radius
  Radius ->
  -- | Initial surface thickness
  Thickness ->
  Problem Energy Temperature (Surface o i Point2D Point2D)
surfaceProblem radius thickness =
  let initialSurf = circularSurface2D @o @i 0 0 radius thickness
      initialGrayMatterArea = area initialSurf.outer - area initialSurf.inner
   in Problem
        { initial = const initialSurf,
          neighbor = modifySurf @avg (40, -40),
          fitness = freeEnergy initialGrayMatterArea,
          schedule = sched,
          acceptance = \_ _ -> acceptanceProbability
        }

sched :: Integer -> Temperature
sched i =
  let progression = 1.0 - fromIntegral i / 200.0
   in if progression < 0.0
        then 0.0
        else Temperature progression

freeEnergy :: forall o i. KnownNat o => KnownNat i => Double -> Surface o i Point2D Point2D -> Energy
freeEnergy initialGrayMatter surf = Energy $ whiteMatter + (1.0 + grayMatterStretch) ** 2.0
  where
    whiteMatter = area surf.inner
    grayMatter = abs $ area surf.outer - whiteMatter
    grayMatterStretch = abs $ (grayMatter - initialGrayMatter) ** 2.0

acceptanceProbability :: Energy -> Energy -> Temperature -> Probability
acceptanceProbability energyState energyNeighbor temperature
  | energyNeighbor < energyState = 1.0
  | otherwise = Probability $ exp (unEnergy (energyState - energyNeighbor) / unTemperature temperature)

seed :: SMGen
seed = mkSMGen 1234
