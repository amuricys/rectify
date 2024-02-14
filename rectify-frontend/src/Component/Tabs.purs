module Component.Tabs where

import Prelude

import CSS as CSS
import CSS.Cursor as CSS.Cursor
import CSS.Overflow as CSS.Overflow
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.CSS as HCSS
import Halogen.HTML.Events as HE

data ActiveTab = Surface | TSP | Reservoir

derive instance Eq ActiveTab

data Action = SurfaceClicked | TSPClicked | ReservoirClicked
type Output = Unit

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
  { initialState: const Surface
  , render: render
  , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
  }
  where
  render :: forall slots. ActiveTab -> H.ComponentHTML Action slots m
  render s = HH.div [ HCSS.style tabArrayStyle ]
    [ mkButton Surface "2D Surface" SurfaceClicked
    , mkButton TSP "Traveling Salesman" TSPClicked
    , mkButton Reservoir "Reservoir computer" ReservoirClicked
    ]
    where
    mkButton tab title action =
      HH.button
        [ HE.onClick \_ -> action
        , HCSS.style $ if s == tab then buttonStyle *> CSS.backgroundColor (CSS.graytone 0.8) else buttonStyle
        ]
        [ HH.text title ]

handleAction :: forall slots m. Action -> H.HalogenM ActiveTab Action slots Output m Unit
handleAction = case _ of
  SurfaceClicked -> H.modify_ \_ -> Surface
  TSPClicked -> H.modify_ \_ -> TSP
  ReservoirClicked -> H.modify_ \_ -> Reservoir