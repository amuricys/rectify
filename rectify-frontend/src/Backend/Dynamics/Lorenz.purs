module Backend.Dynamics.Lorenz where

import Prelude


data State = State {
  x :: Number,
  y :: Number,
  z :: Number
}

data Params = Params {
  sigma :: Number,
  rho :: Number,
  beta :: Number
}
