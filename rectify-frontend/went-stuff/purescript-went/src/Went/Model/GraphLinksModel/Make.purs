module Went.Model.GraphLinksModel.Make where

import Prelude

import GoJS.Model.GraphLinksModel.Constructors (newGraphLinksModel)
import GoJS.Model.Types (GraphLinksModel_)
import Went.Model.Make (class ModelM, MakeModel, model')

graphLinksModel :: forall m nodeData linkData b. ModelM m nodeData linkData => MakeModel (GraphLinksModel_ nodeData linkData) nodeData linkData b -> m Unit
graphLinksModel = model' newGraphLinksModel
