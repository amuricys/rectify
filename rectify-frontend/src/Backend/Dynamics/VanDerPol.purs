module Backend.Dynamics.VanDerPol where

import Prelude

data Params = Params {
  mu :: Number
}

data State = State {
  x :: Number,
  y :: Number
}