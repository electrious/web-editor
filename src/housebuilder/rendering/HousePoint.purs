module HouseBuilder.Rendering.HousePoint where

import Prelude

import Data.Default (def)
import Data.Filterable (filter)
import Data.Lens (Lens', (.~), (^.))
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Maybe (fromMaybe)
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple(..))
import Editor.Common.Lenses (_isDragging, _position, _tapped)
import FRP.Event (Event, withLast)
import Model.Hardware.PanelModel (_isActive)
import Model.HouseEditor.HousePoint (RidgePoint, _ridgePointPos)
import Rendering.NodeRenderable (class NodeRenderable)
import Three.Core.Geometry (Geometry)
import Three.Math.Vector (Vector3)
import UI.DraggableObject (DragObjCfg, createDraggableObject)

newtype RidgePointRendered = RidgePointRendered {
    ridgePoint :: RidgePoint,
    deleted    :: Event RidgePoint,
    dragging   :: Event (Tuple RidgePoint Vector3),
    dragEnd    :: Event Unit
    }

derive instance newtypeRidgePointRendered :: Newtype RidgePointRendered _

_ridgePoint :: forall t a r. Newtype t { ridgePoint :: a | r } => Lens' t a
_ridgePoint = _Newtype <<< prop (SProxy :: SProxy "ridgePoint")

_dragEnd :: forall t a r. Newtype t { dragEnd :: a | r } => Lens' t a
_dragEnd = _Newtype <<< prop (SProxy :: SProxy "dragEnd")

getEndEvt :: Event Boolean -> Event Unit
getEndEvt = map (const unit) <<< filter f <<< withLast
    where f { last, now } = let l = fromMaybe false last in l && not now

instance nodeRenderableRidgePoint :: NodeRenderable e RidgePoint RidgePointRendered where
    render p = do
        let cfg = def # _isActive .~ pure true
                      # _position .~ p ^. _ridgePointPos
        dragObj <- createDraggableObject (cfg :: DragObjCfg Geometry)
        pure $ RidgePointRendered {
            ridgePoint : p,
            deleted    : const p <$> dragObj ^. _tapped,
            dragging   : Tuple p <$> dragObj ^. _position,
            dragEnd    : getEndEvt $ dragObj ^. _isDragging
            }
