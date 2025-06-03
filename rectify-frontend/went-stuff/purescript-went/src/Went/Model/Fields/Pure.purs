module Went.Model.Fields.Pure where

type ModelPureFields (this :: Type) (nodeData :: Row Type) (r :: Row Type) =
  ( copiesArrayObjects :: Boolean
  , copiesArrays :: Boolean
  , copiesKey :: Boolean
  , copyNodeDataFunction :: Record nodeData -> this -> Record nodeData
  , dataFormat :: String
  , isReadOnly :: Boolean
  -- TODO:  missing: makeUniqueKeyFunction, modelData, nodeKeyProperty, undoManager
  , name :: String
  , nodeDataArray :: Array (Record nodeData)
  , skipsUndoManager :: Boolean
  | r
  )
