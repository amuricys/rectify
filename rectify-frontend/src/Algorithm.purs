module Algorithm where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)

data Algorithm = Surface | TSP | Reservoir

derive instance Eq Algorithm
derive instance Generic Algorithm _
instance Show Algorithm where
  show = genericShow