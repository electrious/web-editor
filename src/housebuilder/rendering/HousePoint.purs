module HouseBuilder.Rendering.HousePoint where

import Prelude

import Data.Default (def)
import Data.Lens ((^.), (.~))
import Data.Newtype (class Newtype)
import Editor.Common.Lenses (_isDragging, _position, _tapped)
import FRP.Event (Event)
import Model.Hardware.PanelModel (_isActive)
import Model.HouseEditor.HousePoint (RidgePoint, _ridgePointPos)
import Rendering.NodeRenderable (class NodeRenderable)
import Three.Core.Geometry (Geometry)
import Three.Math.Vector (Vector3)
import UI.DraggableObject (DragObjCfg, createDraggableObject)

newtype RidgePointRendered = RidgePointRendered {
    tapped     :: Event Unit,
    position   :: Event Vector3,
    isDragging :: Event Boolean
    }

derive instance newtypeRidgePointRendered :: Newtype RidgePointRendered _

instance nodeRenderableRidgePoint :: NodeRenderable e RidgePoint RidgePointRendered where
    render p = do
        let cfg = def # _isActive .~ pure true
                      # _position .~ p ^. _ridgePointPos
        dragObj <- createDraggableObject (cfg :: DragObjCfg Geometry)
        pure $ RidgePointRendered {
            tapped     : const unit <$> dragObj ^. _tapped,
            position   : dragObj ^. _position,
            isDragging : dragObj ^. _isDragging
            }
