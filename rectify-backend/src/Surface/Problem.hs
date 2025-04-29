{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DerivingStrategies #-}

module Surface.Problem where

import Prelude

import Data.Aeson (FromJSON, ToJSON)
import GHC.TypeLits (KnownNat, type (+), type (<=))
import SimulatedAnnealing (Probability (Probability), Problem (..))
import Surface.Surface (Surface (inner, outer, Surface), Thickness, circularSurface2D, modifySurf)
import Surface.Circular (Radius (unRadius), area)
import Surface.LinAlg (Point2D)
import System.Random.SplitMix (SMGen, mkSMGen)
import Data.Type.Ord (type (<))
import Helpers
import Debug.Pretty.Simple (pTraceShow)
import Config
import Effectful
import Random (RandomEff)

newtype Energy = Energy {unEnergy :: Double}
  deriving newtype (Show, Eq, Num, Ord, Fractional, FromJSON, ToJSON)

newtype Temperature = Temperature {unTemperature :: Double}
  deriving newtype (Show, Eq, Num, Ord, Fractional, FromJSON, ToJSON)

surfaceProblem ::
  forall o i avg oprev iprev es.
  ConfigEff :> es =>
  RandomEff :> es =>
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
  Problem es Energy Temperature (Surface Point2D Point2D)
surfaceProblem radius thickness =
  let initialSurf = circularSurface2D @o @i 0 0 radius thickness
      initialGrayMatterArea = case initialSurf of
        -- this pattern matching is needed so the constraints on outer and inner are brought into scope
        Surface outer inner -> area outer - area inner
   in Problem
        { initial = pure initialSurf,
          neighbor = modifySurf @avg,
          fitness = freeEnergy initialGrayMatterArea,
          schedule = sched,
          acceptance = \_ _ x y t -> pure $ acceptanceProbability x y t
        }

sched :: Integer -> Temperature
sched i =
  let progression = 1000.0 - fromIntegral i / 200.0
   in if progression < 0.0
        then 0.0
        else Temperature progression

freeEnergy :: Double -> Surface Point2D Point2D -> Energy
freeEnergy initialGrayMatter (Surface inner outer) = Energy $ whiteMatter + (1.0 + grayMatterStretch) ** 2.0
  where
    whiteMatter = area inner
    grayMatter = abs $ area outer - whiteMatter
    delta = grayMatter - initialGrayMatter
    grayMatterStretch = abs (delta * delta)

acceptanceProbability :: Energy -> Energy -> Temperature -> Probability
acceptanceProbability energyState energyNeighbor temperature
  | energyNeighbor < energyState = 1.0
  | temperature <= 0.0 = 1.0
  | otherwise = Probability $ exp (unEnergy (energyState - energyNeighbor) / unTemperature temperature)

seed :: SMGen
seed = mkSMGen 1234
