module Component.Energy where

import Prelude

import Data.Maybe
import Halogen as H
import Halogen.HTML as HH

data Query a = SetEnergy Number a

newtype EnergyState = EnergyState { energy :: Number }

handleQuery :: forall m a. Query a -> H.HalogenM EnergyState Unit () Void m (Maybe a)
handleQuery = case _ of
  SetEnergy e a -> H.modify_ (\(EnergyState x) -> EnergyState x { energy = e }) $> Just a

component ∷ forall output q m. H.Component q Unit output m
component = H.mkComponent
  { initialState: const (EnergyState { energy: 0.0 })
  , render: render
  , eval: H.mkEval H.defaultEval }

render :: forall mon slot s. s -> H.ComponentHTML Unit mon slot
render _ = HH.text "Energy"

