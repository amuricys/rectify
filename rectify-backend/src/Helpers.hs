module Helpers where

import Prelude

import Debug.Pretty.Simple (pTraceShow)

tap :: Show b => b -> b
tap x = pTraceShow x x