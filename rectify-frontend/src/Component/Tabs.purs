module Component.Tabs where

import Prelude

import Algorithm (Algorithm(..))
import CSS as CSS
import CSS.Cursor as CSS.Cursor
import CSS.Overflow as CSS.Overflow
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.CSS as HCSS
import Halogen.HTML.Events as HE

newtype Action = Clicked Algorithm
newtype Output = Selected Algorithm
newtype State = Current Algorithm

tabArrayStyle :: CSS.StyleM Unit
tabArrayStyle = do
  CSS.Overflow.overflow CSS.Overflow.hidden
  CSS.border CSS.solid (CSS.px 1.0) (CSS.graytone 0.8)
  CSS.backgroundColor (CSS.graytone 0.94)

buttonStyle :: CSS.StyleM Unit
buttonStyle = do
  CSS.backgroundColor (CSS.graytone 0.94)
  CSS.float CSS.floatLeft
  CSS.border CSS.solid (CSS.px 0.0) (CSS.hsla 0.0 0.0 0.0 0.0) -- complicated versions
  CSS.outline CSS.solid (CSS.px 0.0) (CSS.hsla 0.0 0.0 0.0 0.0) -- of "none" arguments
  CSS.cursor CSS.Cursor.pointer
  CSS.padding (CSS.px 14.0) (CSS.px 16.0) (CSS.px 14.0) (CSS.px 16.0)
  CSS.transition "background-color" (CSS.sec 0.3) CSS.easeInOut (CSS.sec 0.0)

component :: ∀ (m :: Type -> Type) (query :: Type -> Type) (a :: Type). H.Component query a Output m
component = H.mkComponent
  { initialState: const (Current Surface)
  , render: render
  , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
  }
  where
  render :: forall slots. State -> H.ComponentHTML Action slots m
  render (Current s) = HH.div [ HCSS.style tabArrayStyle ]
    [ mkButton Surface "2D Surface" (Clicked Surface)
    , mkButton TSP "Traveling Salesman" (Clicked TSP)
    , mkButton Reservoir "Reservoir computer" (Clicked Reservoir)
    ]
    where
    mkButton tab title action =
      HH.button
        [ HE.onClick \_ -> action
        , HCSS.style $ if s == tab then buttonStyle *> CSS.backgroundColor (CSS.graytone 0.8) else buttonStyle
        ]
        [ HH.text title ]

handleAction :: forall slots m. Action -> H.HalogenM State Action slots Output m Unit
handleAction (Clicked alg) = do
  H.modify_ <<< const $ Current alg
  H.raise (Selected alg)
