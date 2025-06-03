module Went.GraphObject.Panel.Fields.ExtraChild where

import GoJS.GraphObject.Types (Adornment_, Link_, Shape_, TextBlock_)
import Went.Geometry.Point (Point)
import Went.GraphObject.EnumValue.SegmentOrientation (SegmentOrientation)
import Went.GraphObject.Panel.PanelType (Auto', Graduated', Grid', Link', Spot', Table', TableColumn', TableRow')
import Went.GraphObject.Shape.Arrowhead (Arrowhead)

class ExtraFieldsChild (panelWithType :: Type) (child :: Type) (extraFields :: Row Type) | panelWithType child -> extraFields

instance
  ExtraFieldsChild (tag Table' panel)
    anychild
    ( row :: Int
    , rowSpan :: Int
    , column :: Int
    , columnSpan :: Int
    )
else instance
  ExtraFieldsChild (tag Link' Link_)
    Shape_
    ( segmentFraction :: Number
    , segmentIndex :: Int
    , segmentOffset :: Point
    , segmentOrientation :: SegmentOrientation
    , isPanelMain :: Boolean
    , toArrow :: Arrowhead
    , fromArrow :: Arrowhead
    )
else instance linkAnyChild ::
  ExtraFieldsChild (tag Link' Link_)
    anychild
    ( segmentFraction :: Number
    , segmentIndex :: Int
    , segmentOffset :: Point
    , segmentOrientation :: SegmentOrientation
    , isPanelMain :: Boolean
    )
else instance linkAdornmentAnyChild ::
  ExtraFieldsChild (tag Link' Adornment_)
    anychild
    ( isPanelMain :: Boolean
    )
else instance autoAnyChild ::
  ExtraFieldsChild (tag Auto' panel)
    anychild
    ( isPanelMain :: Boolean
    )
else instance spotAnyChild ::
  ExtraFieldsChild (tag Spot' panel)
    anychild
    ( isPanelMain :: Boolean
    )
else instance tableRowAnyChild ::
  ExtraFieldsChild (tag TableRow' panel)
    anychild
    ( isPanelMain :: Boolean
    )
else instance tableColumnAnyChild ::
  ExtraFieldsChild (tag TableColumn' panel)
    anychild
    ( isPanelMain :: Boolean
    )
else instance graduatedShapeChild ::
  ExtraFieldsChild (tag Graduated' panel)
    Shape_
    ( segmentOrientation :: SegmentOrientation
    , graduatedEnd :: Number
    , graduatedSkip :: Number -> Shape_ -> Boolean
    , graduatedStart :: Number
    , interval :: Number
    , isPanelMain :: Boolean
    )
else instance gridShapeChild ::
  ExtraFieldsChild (tag Grid' panel)
    Shape_
    ( interval :: Number
    )
else instance graduatedTextBlockChild ::
  ExtraFieldsChild (tag Graduated' panel)
    TextBlock_
    ( segmentOrientation :: SegmentOrientation
    , graduatedEnd :: Number
    , graduatedFunction :: Number -> TextBlock_ -> String
    , graduatedSkip :: Number -> TextBlock_ -> Boolean
    , graduatedStart :: Number
    , interval :: Number
    , isPanelMain :: Boolean
    )
else instance gridTextBlockChild ::
  ExtraFieldsChild (tag Grid' panel)
    TextBlock_
    ( interval :: Number
    )
else instance graduatedAnyChild ::
  ExtraFieldsChild (tag Graduated' panel)
    anychild
    ( segmentOrientation :: SegmentOrientation
    , isPanelMain :: Boolean
    )
else instance anyOtherPanelTypeAnyChild ::
  ExtraFieldsChild k anychild ()