{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}

module SimulatedAnnealing.Surface.Problem where

import Data.Aeson (FromJSON, ToJSON)
import Data.Finite
import Data.Proxy (Proxy (..))
import Data.Type.Ord (type (<))
import Data.Vector.Sized ((++), (//))
import Data.Vector.Sized qualified as V
import Effectful
import Effectful.Reader.Dynamic (Reader, ask)
import GHC.TypeLits (KnownNat, natVal, type (+), type (<=))
import Random (RandomEff, nextDouble, nextInteger)
import SimulatedAnnealing (Probability (Probability), Problem (..))
import SimulatedAnnealing.Surface.Change (Change (..), applyChange, linear, pushInners, smooth)
import SimulatedAnnealing.Surface.Circular (area, toCircularLines, Radius)
import SimulatedAnnealing.Surface.Config
import Util.LinAlg (Point2D, X (X), Y (Y), linesIntersection)
import SimulatedAnnealing.Surface.Surface2D (Surface (Surface, inner, outer), circularSurface2D, surfRemoved, surfAdded, Thickness)
import System.Random.SplitMix (SMGen, mkSMGen)
import Prelude hiding ((++))
import Control.Concurrent.STM (TVar, newTVarIO, modifyTVar, atomically)
import GHC.IO (unsafePerformIO)
import Control.Concurrent.STM.TVar (readTVarIO, readTVar)

newtype Energy = Energy {unEnergy :: Double}
  deriving newtype (Show, Eq, Num, Ord, Fractional, FromJSON, ToJSON)

newtype Temperature = Temperature {unTemperature :: Double}
  deriving newtype (Show, Eq, Num, Ord, Fractional, FromJSON, ToJSON)

surfaceProblem ::
  forall o i avg oprev iprev es.
  (Reader Config :> es) =>
  (RandomEff :> es) =>
  (i ~ iprev + 1) =>
  (o ~ oprev + 1) =>
  (avg < i) =>
  (KnownNat o) =>
  (KnownNat i) =>
  (KnownNat avg) =>
  -- | Surface radius
  Radius ->
  -- | Initial surface thickness
  Thickness ->
  Problem (Eff es) Energy Temperature (Surface Point2D Point2D)
-- TODO: Would be nice if this already ran in the reader monad so radius and thickness aren't passed
surfaceProblem radius thickness =
  let initialSurf = circularSurface2D @o @i 0 0 radius thickness
      initialGrayMatterArea = case initialSurf of
        -- this pattern matching is needed so the constraints on outer and inner are brought into scope
        Surface outer inner -> area outer - area inner
   in Problem
        { initial = pure initialSurf,
          neighbor = pushSurface @avg,
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
  | otherwise = 0.0
  -- | temperature <= 0.0 = 1.0
  -- | otherwise = Probability $ exp (unEnergy (energyState - energyNeighbor) / unTemperature temperature)

seed :: SMGen
seed = mkSMGen 12345

pushSurface ::
  forall avg es.
  ( KnownNat avg,
    RandomEff :> es,
    Reader Config :> es
  ) =>
  Surface Point2D Point2D ->
  Eff es (Surface Point2D Point2D)
pushSurface s@(Surface (outer :: _Vector o Point2D) (inner :: _Vector i Point2D)) = do
  -- Choose a random point on the outer surface (- 1 because nextInteger interval is closed)
  Config {changeRange, addThresh, removeThresh} <- ask
  i <- finite <$> nextInteger 0 (natVal (Proxy @o) - 1)
  xChange <- X . (`subtract` changeRange) . (* (2 * changeRange)) <$> nextDouble
  yChange <- Y . (`subtract` changeRange) . (* (2 * changeRange)) <$> nextDouble
  let smoothedOuterChanges = smooth @8 i (Change xChange yChange) linear
      smoothedInnerChanges = pushInners @avg outer inner smoothedOuterChanges
      appliedOuter = outer // (applyChange outer <$> V.toList smoothedOuterChanges)
      appliedInner = inner // (applyChange inner <$> smoothedInnerChanges)
  pure case surfRemoved removeThresh appliedOuter appliedInner of
    (Surface outer' inner') ->
      case linesIntersection . V.toList $ toCircularLines outer' ++ toCircularLines inner' of
        Just p -> s
        Nothing -> surfAdded addThresh outer' inner'
