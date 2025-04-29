module Config where

import Prelude
import Effectful.Reader.Dynamic (Reader)

data HL = HL { high :: Double, low :: Double}

data Config = Config
  { highLow      :: HL
  , addThresh    :: Double
  , removeThresh :: Double
  , smoothRange  :: Int
  -- etc.
  }

type ConfigEff = Reader Config
