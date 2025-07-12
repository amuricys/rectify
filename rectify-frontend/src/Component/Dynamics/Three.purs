module Component.Three where

import Prelude

import Effect (Effect)

import CSS as CSS
import Data.Maybe (Maybe(..))
import Effect.Class (class MonadEffect, liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.CSS as HCSS
import Halogen.HTML.Properties as HP


foreign import data Renderer :: Type
foreign import data Scene :: Type
foreign import data Camera :: Type
foreign import data RAFId :: Type


type ThreeEnvironment = { renderer :: Renderer, scene :: Scene, camera :: Camera, rafId :: RAFId }

foreign import initThree :: Effect ThreeEnvironment
foreign import addPoints :: forall pointType. Scene -> Array pointType -> Effect Unit
foreign import pauseThree :: Renderer -> Effect Unit
foreign import resumeThree :: Renderer -> Effect Unit
foreign import disposeThree :: Effect Unit


data Action = Initialize
data Query pointType a = AddPoints (Array pointType) a

type State = { threeEnvironment :: Maybe ThreeEnvironment }


component
  :: forall input m pointType
   . MonadEffect m
  => H.Component (Query pointType) input Void m
component = H.mkComponent
  { initialState: const { threeEnvironment: Nothing }
  , render
  , eval: H.mkEval $ H.defaultEval
      { handleAction = handleAction
      , handleQuery = handleQuery
      , initialize = Just Initialize
      }
  }
  where
  render _ =
    HH.div
      [ HP.id "three-canvas"
      , HCSS.style do
          CSS.height (CSS.pct 100.0)
      ]
      [  ]

handleQuery :: forall m pointType a. MonadEffect m => Query pointType a -> H.HalogenM State Action () Void m (Maybe a)
handleQuery = case _ of
  AddPoints points a -> do
    env <- H.gets _.threeEnvironment
    case env of
      Just env' -> liftEffect $ addPoints env'.scene points *> pure (Just a)
      Nothing -> pure (Just a)

handleAction :: forall m. MonadEffect m => Action -> H.HalogenM State Action () Void m Unit
handleAction = case _ of
  Initialize -> initThreeEnvironment

initThreeEnvironment :: forall m. MonadEffect m => H.HalogenM State Action () Void m Unit
initThreeEnvironment = do
  -- TODO: 1. Clear existing diagram before this, then 2. Make different diagram for different algorithms
  d <- liftEffect $ initThree
  H.modify_ _ { threeEnvironment = Just d } 