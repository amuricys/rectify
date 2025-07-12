module Component.Optimization.RunCommand where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)

data RunCommand = Pause | Unpause | Step
derive instance Generic RunCommand _
instance Show RunCommand where
  show = genericShow
