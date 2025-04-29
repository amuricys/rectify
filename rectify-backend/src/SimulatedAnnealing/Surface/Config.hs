{-# LANGUAGE DerivingStrategies #-}
module SimulatedAnnealing.Surface.Config where

import Prelude
import Data.Aeson (FromJSON, ToJSON)
import SimulatedAnnealing.Surface.Surface2D
import SimulatedAnnealing.Surface.Circular (Radius)


data Config = Config
  { changeRange  :: Double
  , addThresh    :: Double
  , removeThresh :: Double
  , thickness    :: Thickness
  , radius       :: Radius
  -- etc.
  }
