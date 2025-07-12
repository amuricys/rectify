module Component.Parent where

import Prelude

import Component.Banner as Banner
import Component.Optimization as Optimization
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Halogen (Slot)
import Halogen as H
import Halogen.HTML as HH
import Type.Prelude (Proxy(..))

type ComponentState = Unit

-- Constants to avoid mixing up indices and proxies
inds :: { banner :: Int , optimization :: Int, dynamics :: Int }
inds = {banner: 0, optimization: 1, dynamics: 2}

_banner :: Proxy "banner"
_banner = Proxy

_optimization :: Proxy "optimization"
_optimization = Proxy

_dynamics :: Proxy "dynamics"
_dynamics = Proxy

component :: forall input q m. MonadAff m => H.Component q input Void m
component =
  H.mkComponent
    { initialState: const unit
    , render
    , eval: H.mkEval H.defaultEval
    }


type Slots q =
  ( banner :: Slot q Void Int
  , optimization :: Slot q Void Int
  , dynamics :: Slot q Void Int
  )

render :: forall m s q a. MonadEffect m => s -> H.ComponentHTML a (Slots q) m
render _ =
  HH.div_ -- Main container
    [ HH.slot_ _banner inds.banner Banner.component unit -- Banner remains at the top
    , HH.slot_ _optimization inds.optimization Optimization.component unit
    -- , HH.slot_ _dynamics inds.dynamics Three.component unit
    ]
