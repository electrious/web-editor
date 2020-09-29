module Editor.UI.DragInfo where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Lens ((^.))
import Data.Newtype (class Newtype)
import Editor.Common.Lenses (_distance, _dragType, _point)
import Editor.Input.Commoon (DragType)
import Editor.SceneEvent (SceneDragEvent)
import Three.Math.Vector (Vector3)

newtype DragInfo a = DragInfo {
    dragType :: DragType,
    point    :: Vector3,
    distance :: Number,
    object   :: a
}

derive instance newtypeDragInfo :: Newtype (DragInfo a) _
derive instance genericDragInfo :: Generic (DragInfo a) _
instance showDragInfo :: Show a => Show (DragInfo a) where
    show = genericShow

mkDragInfo :: forall a. a -> SceneDragEvent -> DragInfo a
mkDragInfo d evt = DragInfo {
    dragType : evt ^. _dragType,
    point    : evt ^. _point,
    distance : evt ^. _distance,
    object   : d
}