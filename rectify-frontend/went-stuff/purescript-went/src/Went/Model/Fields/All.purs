module Went.Model.Fields.All where

import GoJS.Model.Types (GraphLinksModel_, Model_, TreeModel_)
import Went.Model.Fields.Pure (ModelPureFields)
import Went.Model.GraphLinksModel.Fields (GraphLinksModelFields)
import Went.Model.TreeModel.Fields (TreeModelFields)

class ModelFields (ffiType :: Type) (fields :: Row Type) | ffiType -> fields

instance ModelFields (Model_ nodeData) (ModelPureFields (Model_ nodeData) nodeData ())
instance ModelFields (GraphLinksModel_ nodeData linkData) (GraphLinksModelFields nodeData linkData)
instance ModelFields (TreeModel_ nodeData) (TreeModelFields nodeData)
