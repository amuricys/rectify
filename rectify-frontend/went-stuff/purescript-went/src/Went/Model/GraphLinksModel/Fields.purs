module Went.Model.GraphLinksModel.Fields where

import GoJS.Key (Key, KeyProperty)
import GoJS.Model.Types (GraphLinksModel_)
import Went.Model.Fields.Pure (ModelPureFields)

type GraphLinksModelPureFields nodeData linkData =
  ( archetypeNodeData :: Record nodeData
  , linkDataArray :: Array (Record linkData)
  , linkCategoryProperty :: KeyProperty linkData String
  , linkFromKeyProperty :: KeyProperty linkData Key
  , linkFromPortIdProperty :: KeyProperty linkData String
  , linkKeyProperty :: KeyProperty linkData Key
  , linkLabelKeysProperty :: KeyProperty linkData (Array Key)
  , linkToKeyProperty :: KeyProperty linkData Key
  , linkToPortIdProperty :: KeyProperty linkData String
  , makeUniqueLinkKeyFunction :: GraphLinksModel_ nodeData linkData -> Record linkData -> Key
  , nodeGroupKeyProperty :: KeyProperty nodeData Key
  , nodeIsGroupProperty :: KeyProperty nodeData Boolean
  )

type GraphLinksModelFields nodeData linkData = ModelPureFields (GraphLinksModel_ nodeData linkData) nodeData (GraphLinksModelPureFields nodeData linkData)