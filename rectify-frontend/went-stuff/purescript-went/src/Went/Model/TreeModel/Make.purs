module Went.Model.TreeModel.Make where

import Prelude

import GoJS.Model.TreeModel.Constructors (newTreeModel)
import GoJS.Model.Types (TreeModel_)
import Went.Model.Make (class ModelM, MakeModel, model')

treeModel :: forall m nodeData linkData b. ModelM m nodeData linkData => MakeModel (TreeModel_ nodeData) nodeData linkData b -> m Unit
treeModel = model' newTreeModel