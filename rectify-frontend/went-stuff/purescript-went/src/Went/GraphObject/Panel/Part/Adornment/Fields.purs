module Went.GraphObject.Panel.Part.Adornment.Fields where

import GoJS.GraphObject.Types (Adornment_)
import Went.GraphObject.Fields.Pure (GraphObjectPureFields)
import Went.GraphObject.Panel.Fields.Pure (PanelPureFields)
import Went.GraphObject.Panel.Part.Fields.Pure (PartPureFields)

type AdornmentPureFields (a :: Row Type) =
  ( 
    -- Adornments have no specific fields that make sense to set statically.
    | a
  )

type AdornmentFields (extraFields :: Row Type) =
  GraphObjectPureFields Adornment_
    ( PanelPureFields
        ( PartPureFields Adornment_
            ( AdornmentPureFields extraFields
            )
        )
    )
