{-# LANGUAGE TemplateHaskell #-}

module Random where

import Effectful
import Prelude 
import Effectful.TH (makeEffect)
import System.Random.SplitMix (SMGen)
import System.Random.SplitMix qualified as SplitMix
import Effectful.State.Static.Local
import Effectful.Dispatch.Dynamic (reinterpret)
import Data.Word (Word64)
import Debug.Pretty.Simple


data RandomEff :: Effect where
    NextDouble :: RandomEff m Double
    NextInteger :: Integer -> Integer -> RandomEff m Integer
    BitmaskWithRejection :: Word64 -> RandomEff m Word64
makeEffect ''RandomEff

randomBody :: forall s es a. State s :> es => (s -> (a, s)) -> Eff es a
randomBody f = do
  s <- get
  let (d, s') = f s
  put s'
  pure d

runRandomPure :: forall es a. SMGen -> Eff (RandomEff : es) a -> Eff es a
runRandomPure smgen = reinterpret (evalState smgen) $ \_ -> \case
  NextDouble -> randomBody SplitMix.nextDouble
  NextInteger min max -> randomBody (SplitMix.nextInteger min max)
  BitmaskWithRejection word -> randomBody (SplitMix.bitmaskWithRejection64 word)

