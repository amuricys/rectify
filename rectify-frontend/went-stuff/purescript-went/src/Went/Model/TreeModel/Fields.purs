module Went.Model.TreeModel.Fields where

import GoJS.Model.Types (TreeModel_)
import GoJS.Key (Key, KeyProperty)
import Went.Model.Fields.Pure (ModelPureFields)

type TreeModelPureFields nodeData =
  ( nodeParentKeyProperty :: KeyProperty nodeData Key
  , parentLinkCategoryProperty :: KeyProperty nodeData String
  )

type TreeModelFields nodeData = ModelPureFields (TreeModel_ nodeData) nodeData (TreeModelPureFields nodeData)