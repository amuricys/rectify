{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}

module SimulatedAnnealing.TSP.Problem where

-- import Control.DeepSeq (NFData (..))

-- import qualified Data.Vector.Mutable    as MV

import Data.Aeson (ToJSON (toJSON), Value (Object))
import Data.Aeson.KeyMap (fromList)
import Data.Finite (Finite, finite)
import Data.Foldable (toList)
import Data.List qualified as L
import Data.Maybe (fromJust)
import Data.Proxy (Proxy (..))
import Data.Set qualified as Set
import Data.Vector.Mutable.Sized qualified as MV
import Data.Vector.Sized qualified as V
import Debug.Pretty.Simple (pTraceShow)
import Effectful
import GHC.Generics (Generic)
import GHC.TypeLits (KnownNat, Nat)
import Random (RandomEff, bitmaskWithRejection)
import SimulatedAnnealing (Probability (Probability), Problem (..))
import System.Random.SplitMix qualified as SM
import Util.LinAlg
import Prelude

newtype FinnRandoSolution n = FinnRandoSolution {unFinnRandoSolution :: V.Vector n City}
  deriving (Eq, Ord, Show, Generic)

instance ToJSON (FinnRandoSolution n) where
  -- Disgusting haha
  toJSON (FinnRandoSolution v) =
    Object . fromList $ [("tag", "TSPSolution"), ("values", toJSON [vals])]
    where
      vals = fromList [("cities", V.toList . fmap (point2Json . pointBimap ad ad . cityWGS84) $ v)]
      point2Json (Point2D x y) = Object . fromList $ [("x", toJSON x), ("y", toJSON y)]
      ad x = (x - 50) * 50.0

-- instance NFData FinnRandoSolution where
--     rnf (FinnRandoSolution xs) = rnf xs

-- |
--
-- \[
-- A(\mu, \nu) = \begin{cases}
-- e^{-\beta (H_\nu - H_\mu)}, &\text{if } H_\nu - H_\mu > 0 \\
-- 1 & \text{otherwise}
-- \end{cases}
-- \]
acceptanceProbability :: Double -> Double -> Double -> Probability
acceptanceProbability energyState energyNeighbor temperature
  | energyNeighbor < energyState = 1.0
  | otherwise = min 1.0 $ Probability $ exp ((energyState - energyNeighbor) / temperature)

isingAcceptance ::
  -- | energyState
  Double ->
  -- | energyNeighbor
  Double ->
  -- | temperature
  Double ->
  Probability
isingAcceptance energyState energyNeighbor beta
  | di < 0 = Probability $ exp (negate (beta * di))
  | otherwise = 1
  where
    di = energyNeighbor - energyState

solutionToList :: FinnRandoSolution n -> [City]
solutionToList (FinnRandoSolution xs) = V.toList xs

type Metric = Double

type Beta = Double

routeDistance :: (Foldable f) => f City -> Metric
routeDistance = L.foldl' (+) 0 . map (uncurry cityDistance) . pairs . toList

tspProblem ::
  forall n es.
  (KnownNat n) =>
  (RandomEff :> es) =>
  -- | steps
  Int ->
  Set.Set City ->
  Problem (Eff es) Metric Beta (FinnRandoSolution n)
tspProblem steps cities = Problem {initial, neighbor, fitness, schedule, acceptance = \_ _ x y z -> pure $ acceptanceProbability x y z}
  where
    -- start and end kilometer changes
    km0, km1 :: Double
    km0 = 2000
    km1 = 3

    schedule step = max (m - delta) 0
      where
        m = (km1 - km0) + km0
        delta = fromIntegral (step + 1) / fromIntegral steps * m

    initial = FinnRandoSolution . fromJust . V.fromList <$> go cities
      where
        go rest
          | Set.null rest = pure []
          | otherwise = do
              w <- bitmaskWithRejection (fromIntegral (Set.size rest))
              let c = Set.elemAt (fromIntegral w) rest
              (c :) <$> go (Set.delete c rest)

    acceptance _ _ x y z = pure $ isingAcceptance x y z

    fitness :: FinnRandoSolution n -> Metric
    fitness = routeDistance . solutionToList

    neighbor :: FinnRandoSolution n -> Eff es (FinnRandoSolution n)
    neighbor (FinnRandoSolution s) = do
      let size = V.length s
      i <- finite . fromIntegral <$> bitmaskWithRejection (fromIntegral size)
      j <- finite . fromIntegral <$> bitmaskWithRejection (fromIntegral size)
      -- j'       = mod (i' + 1) (fromIntegral $ V.length s)

      -- f mv = do
      --     x <- MV.unsafeRead mv i
      --     y <- MV.unsafeRead mv j
      --     MV.unsafeWrite mv i y
      --     MV.unsafeWrite mv j x
      let at_i = s `V.index` i
          at_j = s `V.index` j
      pure . FinnRandoSolution $ s V.// [(j, at_i), (i, at_j)]

-- loop pairs.
pairs :: [a] -> [(a, a)]
pairs [] = []
pairs xs@(x : xs') = zip xs (xs' ++ [x])

-------------------------------------------------------------------------------
-- Data
-------------------------------------------------------------------------------

-- | One of 30 biggest cities in Finland. (they are quite small).
data City
  = Helsinki
  | Espoo
  | Tampere
  | Vantaa
  | Oulu
  | Turku
  | Jyvaskyla
  | Lahti
  | Kuopio
  | Pori
  | Kouvola
  | Joensuu
  | Lappeenranta
  | Hameenlinna
  | Vaasa
  | Seinajoki
  | Rovaniemi
  | Mikkeli
  | Kotka
  | Salo
  | Porvoo
  | Kokkola
  | Hyvinkaa
  | Lohja
  | Jarvenpaa
  | Rauma
  | Kajaani
  | Kerava
  | Savonlinna
  | Nokia
  deriving (Eq, Ord, Show, Enum, Bounded)

allCities = Set.fromList [Helsinki .. Nokia]

-- TODO: What are these NFData instances doing?
-- instance NFData City where
--     rnf x = x `seq` ()

-- | WGS84 coordinates of 'City'.
cityWGS84 :: City -> Point2D
cityWGS84 Helsinki = Point2D 60.166641 24.943537
cityWGS84 Espoo = Point2D 60.206376 24.656729
cityWGS84 Tampere = Point2D 61.497743 23.76129
cityWGS84 Vantaa = Point2D 60.298134 25.006641
cityWGS84 Oulu = Point2D 65.013785 25.472099
cityWGS84 Turku = Point2D 60.45169 22.266867
cityWGS84 Jyvaskyla = Point2D 62.241678 25.749498
cityWGS84 Lahti = Point2D 60.980381 25.654988
cityWGS84 Kuopio = Point2D 62.892983 27.688935
cityWGS84 Pori = Point2D 61.483726 21.7959
cityWGS84 Kouvola = Point2D 60.866825 26.705598
cityWGS84 Lappeenranta = Point2D 61.05875 28.18769
cityWGS84 Joensuu = Point2D 62.602079 29.759679
cityWGS84 Hameenlinna = Point2D 60.996174 24.464425
cityWGS84 Vaasa = Point2D 63.092589 21.615874
cityWGS84 Seinajoki = Point2D 62.786663 22.84228
cityWGS84 Rovaniemi = Point2D 66.50279 25.728479
cityWGS84 Mikkeli = Point2D 61.687727 27.273224
cityWGS84 Kotka = Point2D 60.465521 26.941153
cityWGS84 Salo = Point2D 60.384374 23.126727
cityWGS84 Porvoo = Point2D 60.395372 25.66656
cityWGS84 Kokkola = Point2D 63.837583 23.131962
cityWGS84 Hyvinkaa = Point2D 60.631017 24.861124
cityWGS84 Lohja = Point2D 60.250916 24.065782
cityWGS84 Jarvenpaa = Point2D 60.481098 25.100747
cityWGS84 Rauma = Point2D 61.128738 21.511127
cityWGS84 Kajaani = Point2D 64.226734 27.728047
cityWGS84 Kerava = Point2D 60.404869 25.103549
cityWGS84 Savonlinna = Point2D 61.869803 28.878498
cityWGS84 Nokia = Point2D 61.478774 23.508499

-- | Distance between cities.
--
-- Computed using WGS84 model, assuming height 0.
cityDistance :: City -> City -> Double
cityDistance c1 c2 = norm (cityWGS84 c2 `sub` cityWGS84 c1)
