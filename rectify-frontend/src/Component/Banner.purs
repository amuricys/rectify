module Component.Banner where

import Prelude

import CSS as CSS
import CSS.Common as CSS.Common
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.CSS as HCSS

component :: ∀ (m :: Type -> Type) (query :: Type -> Type) (a :: Type). H.Component query a Void m
component = H.mkComponent
  { initialState: const unit
  , render: render
  , eval: H.mkEval $ H.defaultEval
  }
  where
  render :: forall s slots. s -> H.ComponentHTML a slots m
  render _ = HH.div
    [ HCSS.style do
        CSS.height (CSS.px 100.0)
        CSS.width (CSS.pct 100.0)
        CSS.display CSS.flex
        CSS.flexDirection CSS.row
        CSS.justifyContent CSS.flexStart
        CSS.alignItems CSS.Common.center
        CSS.backgroundColor (CSS.rgb 170 0 170)
    ]
    [ HH.div
        [ HCSS.style do
            CSS.marginLeft (CSS.px 20.0)
            CSS.fontSize (CSS.px 24.0)
            CSS.color (CSS.rgb 200 200 5)
            -- TODO: Add Ada Beat font
        ]
        [ HH.text "Went demo" ]
    ]
