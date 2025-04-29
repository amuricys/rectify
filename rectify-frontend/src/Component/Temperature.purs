module Component.Temperature where

import Prelude

import Data.Maybe
import Halogen as H
import Halogen.HTML as HH

data Query a = SetTemperature Number a

component = H.mkComponent
  { initialState: const unit
  , render: render 
  , eval: H.mkEval H.defaultEval }

render :: forall mon slot s. s -> H.ComponentHTML Unit mon slot
render _ = HH.text "Temperature"

newtype TemperatureState = TemperatureState { temperature :: Number }

handleQuery :: forall m a. Query a -> H.HalogenM TemperatureState Unit () Void m (Maybe a)
handleQuery = case _ of
  SetTemperature t a -> H.modify_ (\(TemperatureState x) -> TemperatureState x { temperature = t }) $> Just a
