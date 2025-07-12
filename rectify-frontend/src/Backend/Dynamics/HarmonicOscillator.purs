module Backend.Dynamics.HarmonicOscillator where

import Prelude

data State = State {
  x :: Number,
  y :: Number
}

data Params = HarmonicOscillatorParams {
  omega :: Number,
  x0 :: Number,
  y0 :: Number
}