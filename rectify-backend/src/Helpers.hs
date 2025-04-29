module Helpers where

import Prelude

import Debug.Pretty.Simple (pTraceShow)

tap :: Show b => b -> b
tap x = seq (pTraceShow x x) x