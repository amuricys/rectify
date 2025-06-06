module Went.GraphObject.Picture.Fields where

import Prelude

import Effect (Effect)
import GoJS.GraphObject.Types (Picture_)
import Web.Event.Internal.Types (Event)
import Went.Geometry.Rect (Rect)
import Went.Geometry.Spot (Spot)
import Went.GraphObject.Fields.Pure (GraphObjectPureFields)
import Went.GraphObject.EnumValue.Flip (Flip)
import Went.GraphObject.EnumValue.Stretch (Stretch)

{-
element -- Ignored, set the picture through the source field instead
errorFunction
flip
imageAlignment
imageStretch
naturalBounds -- Read-only
source
sourceCrossOrigin
sourceRect
successFunction
-}

type PicturePureFields =
  ( errorFunction :: Picture_ -> Event -> Effect Unit
  , flip :: Flip
  , imageAlignment :: Spot
  , imageStretch :: Stretch
  , source :: String
  , sourceCrossOrigin :: Picture_ -> String
  , sourceRect :: Rect
  , successFunction :: Picture_ -> Event -> Effect Unit
  )

type PictureFields = GraphObjectPureFields Picture_ PicturePureFields
