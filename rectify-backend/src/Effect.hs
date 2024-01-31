{-# LANGUAGE BlockArguments, TemplateHaskell #-}
module Effect where

import RIO

import Effectful (Effect, Eff)
import Effectful.TH (makeEffect)
import System.Random (StdGen, uniformR)
import Effectful.Dispatch.Dynamic (interpret, reinterpret)
import Effectful.State.Static.Local (evalState, get, put)

data Rand :: Effect where
  RandomR :: (Double, Double) -> Rand m Double
makeEffect ''Rand

runSimulation = interpret \_ -> \case
  RandomR range -> pure 10.0

runRandomPure ::
  StdGen ->
  Eff (Rand : es) a ->
  Eff es a
runRandomPure seed = reinterpret (evalState seed) $ \_ -> \case
  RandomR path -> do
    curS <- get @StdGen
    let (num, newS) = uniformR path curS
    put newS
    pure num