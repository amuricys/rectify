module Went.GraphObject.Fields.All where

import GoJS.GraphObject.Types (class IsGraphObject, Adornment_, Button_, Group_, Link_, Node_, Panel_, Part_, Picture_, Placeholder_, Shape_, TextBlock_)
import Prim.Row (class Union)
import Type.Data.List (type (:>), List', Nil')
import Went.GraphObject.Panel.Button.ButtonType (ButtonTypeTag)
import Went.GraphObject.Panel.Button.Fields.Extra (class ExtraFieldsButton)
import Went.GraphObject.Panel.Fields.Extra (class ExtraFieldsPanel)
import Went.GraphObject.Panel.Fields.ExtraChild (class ExtraFieldsChild)
import Went.GraphObject.Panel.Fields.Pure (PanelFields)
import Went.GraphObject.Panel.PanelType (Link', PanelTypeTag)
import Went.GraphObject.Panel.Part.Fields.Pure (PartFields)
import Went.GraphObject.Panel.Part.Adornment.Fields (AdornmentFields)
import Went.GraphObject.Panel.Part.Link.Fields (LinkFields)
import Went.GraphObject.Panel.Part.Node.Fields (NodeFields)
import Went.GraphObject.Panel.Part.Node.Group.Fields (GroupFields)
import Went.GraphObject.Picture.Fields (PictureFields)
import Went.GraphObject.Placeholder.Fields (PlaceholderFields)
import Went.GraphObject.Shape.Fields.Pure (ShapeFields)
import Went.GraphObject.TextBlock.Fields (TextBlockFields)

class IsGraphObject graphObjectType <= GraphObjectAllFields (graphObjectType :: Type) (extraFields :: Row Type) | graphObjectType -> extraFields

instance
  ( ExtraFieldsPanel panelType extra
  ) =>
  GraphObjectAllFields (PanelTypeTag panelType Panel_) (PanelFields extra)

instance
  ( ExtraFieldsPanel panelType extra
  ) =>
  GraphObjectAllFields (PanelTypeTag panelType Part_) (PartFields Part_ extra)

instance
  ( ExtraFieldsPanel panelType extra
  ) =>
  GraphObjectAllFields (PanelTypeTag panelType Node_) (NodeFields Node_ extra)

instance
  ( ExtraFieldsPanel panelType extra
  ) =>
  GraphObjectAllFields (PanelTypeTag panelType Adornment_) (AdornmentFields extra)

instance
  ( ExtraFieldsPanel panelType extra
  ) =>
  GraphObjectAllFields (PanelTypeTag panelType Group_) (GroupFields extra)

instance
  ( ExtraFieldsButton buttonType extraButton
  , ExtraFieldsPanel panelType extraPanel
  , Union extraButton extraPanel extra
  ) =>
  GraphObjectAllFields (ButtonTypeTag buttonType panelType Button_) (PanelFields extra)

instance GraphObjectAllFields Link_ LinkFields
instance GraphObjectAllFields (PanelTypeTag Link' Link_) LinkFields
instance GraphObjectAllFields Shape_ ShapeFields
instance GraphObjectAllFields Placeholder_ PlaceholderFields
instance GraphObjectAllFields TextBlock_ TextBlockFields
instance GraphObjectAllFields Picture_ PictureFields

class GraphObjectChildFields (curGO :: Type) (hierarchy :: List' Type) (extraFields :: Row Type) | curGO hierarchy -> extraFields

instance GraphObjectChildFields k Nil' ()
else instance
  ( ExtraFieldsChild latestPanel curGO childSettable
  ) =>
  GraphObjectChildFields curGO (latestPanel :> keys) childSettable

