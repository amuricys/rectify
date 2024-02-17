module Component.TabbedCanvas where

import Prelude

import CSS as CSS
import CSS.Cursor as CSS.Cursor
import CSS.Overflow as CSS.Overflow
import Data.Maybe (Maybe(..))
import GoJS.Diagram (Diagram_)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.CSS as HCSS
import Halogen.HTML.Events as HE

data ActiveDiagram = Surface | TSP | Reservoir

derive instance Eq ActiveDiagram

type TabbedCanvasState =
  { activeDiagram :: ActiveDiagram
  , surfaceDiagram :: Maybe Diagram_
  , tspDiagram :: Maybe Diagram_
  , reservoirDiagram :: Maybe Diagram_
  }

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
  { initialState: const { activeDiagram: Surface, surfaceDiagram: Nothing, tspDiagram: Nothing, reservoirDiagram: Nothing }
  , render: render
  , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
  }
  where
  render :: forall slots. TabbedCanvasState -> H.ComponentHTML Action slots m
  render s = HH.div [ HCSS.style tabArrayStyle ]
    [ mkButton Surface "2D Surface" SurfaceClicked
    , mkButton TSP "Traveling Salesman" TSPClicked
    , mkButton Reservoir "Reservoir computer" ReservoirClicked
    ]
    where
    mkButton tab title action =
      HH.button
        [ HE.onClick \_ -> action
        , HCSS.style $ if s.activeDiagram == tab then buttonStyle *> CSS.backgroundColor (CSS.graytone 0.8) else buttonStyle
        ]
        [ HH.text title ]

handleAction :: forall slots m. Action -> H.HalogenM TabbedCanvasState Action slots Output m Unit
handleAction = case _ of
  SurfaceClicked -> H.modify_ _ { activeDiagram = Surface }
  TSPClicked -> H.modify_ _ { activeDiagram = TSP }
  ReservoirClicked -> H.modify_ _ { activeDiagram = Reservoir }