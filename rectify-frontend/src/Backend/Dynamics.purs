module Backend.Dynamics where

import Prelude

import Backend.Dynamics.Lorenz as Lorenz
import Backend.Dynamics.Duffing as Duffing
import Backend.Dynamics.VanDerPol as VanDerPol
import Backend.Dynamics.HarmonicOscillator as HarmonicOscillator
import Data.Variant (Variant)

data DynSystem    = Lorenz | Duffing | VanDerPol | Harmonic
type StateRow =
  ( lorenz :: Lorenz.State
  , duffing :: Duffing.State
  , vanDerPol :: VanDerPol.State
  , harmonicOscillator :: HarmonicOscillator.State
  )


type ParamsRow =
  ( lorenz :: Lorenz.Params
  , duffing :: Duffing.Params
  , vanDerPol :: VanDerPol.Params
  , harmonicOscillator :: HarmonicOscillator.Params
  )

type Payload =
  { system :: DynSystem
  , state :: Variant StateRow
  , params :: Variant ParamsRow
  , t :: Number
  }


