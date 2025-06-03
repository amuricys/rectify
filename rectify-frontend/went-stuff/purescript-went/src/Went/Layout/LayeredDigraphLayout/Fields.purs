module Went.Layout.LayeredDigraphLayout.Fields where

import Prelude

import Effect (Effect)
import GoJS.Layout (LayeredDigraphLayout_, LayeredDigraphVertex_)
import GoJS.Layout.Types (LayeredDigraphNetwork_)
import Went.FFI.Override (Override)
import Went.Layout.EnumValue.LayeringOption (LayeringOption)
import Went.Layout.Fields.Pure (LayoutPureFields)
import Went.Layout.LayeredDigraphLayout.AlignOption (AlignOption)
import Went.Layout.LayeredDigraphLayout.PackOption (PackOption)


type LayeredDigraphPureFields =
  ( alignOption :: AlignOption
  , setsPortSpots :: Boolean
  , direction :: Number
  , layeringOption :: LayeringOption
  , packOption :: PackOption
  , layerSpacing :: Number
  , columnSpacing :: Number
  , nodeMinColumnSpace :: Override LayeredDigraphLayout_ (LayeredDigraphVertex_ -> Boolean -> Effect Number)
  , nodeMinLayerSpace :: Override LayeredDigraphLayout_ (LayeredDigraphVertex_ -> Boolean -> Effect Number)
  , assignLayers :: Override LayeredDigraphLayout_ (Effect Unit)
  )

type LayeredDigraphFields = LayoutPureFields LayeredDigraphLayout_ LayeredDigraphNetwork_ LayeredDigraphPureFields
