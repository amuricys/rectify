module Backend.Dynamics.Duffing where

import Prelude

data State = State {
  x :: Number,
  y :: Number
}

data Params = Params {
  a :: Number,
  b :: Number,
  c :: Number
}