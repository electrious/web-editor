module Custom.Mesh where

import Prelude

import Data.Compactable (compact)
import Data.Default (def)
import Data.Lens ((^.), (.~))
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Tuple (Tuple(..))
import Editor.Common.Lenses (_dragDelta, _dragType, _dragged, _mesh, _point)
import Editor.Input.Commoon (DragType(..))
import Editor.SceneEvent (SceneDragEvent, SceneTapEvent, makeDraggable, makeTappable, stopDraggable, stopTappable)
import Effect (Effect)
import FRP.Event (Event, makeEvent, mapAccum)
import FRP.Event.Extra (multicast, performEvent)
import Three.Core.Geometry (Geometry)
import Three.Core.Material (Material)
import Three.Core.Mesh (Mesh, mkMesh)
import Three.Core.Object3D (Object3D, hasParent, parent, worldToLocal)
import Three.Math.Vector (Vector3, mkVec3, (<->))

newtype TappableMesh a = TappableMesh {
    mesh   :: Mesh a,
    tapped :: Event SceneTapEvent
}

derive instance newtypeTappableMesh :: Newtype (TappableMesh a) _

tapEvtOn :: forall a. Mesh a -> Event SceneTapEvent
tapEvtOn m = makeEvent \k -> do
                makeTappable m k
                pure (stopTappable m)

mkTappableMesh :: forall a geo mat. Geometry geo -> Material mat -> Effect (TappableMesh a)
mkTappableMesh geo mat = do
    mesh <- mkMesh geo mat
    pure $ TappableMesh {
        mesh   : mesh,
        tapped : tapEvtOn mesh
    }


-- | process the drag events in the event stream to make sure all drag start
-- with dragStart and end with dragEnd
validateDrag :: Event SceneDragEvent -> Event SceneDragEvent
validateDrag evt = compact (mapAccum f evt false)
    where f e canDrag | e ^. _dragType == DragStart = if canDrag
                                                      then Tuple true Nothing  -- if there's a repeated drag start, omit it
                                                      else Tuple true (Just e)
                      | canDrag && e ^. _dragType == Drag = Tuple true (Just e)
                      | e ^. _dragType == DragEnd = if canDrag
                                                    then Tuple false (Just e) -- stop dragging, send the end event
                                                    else Tuple false Nothing  -- already ended, omit the end event
                      | otherwise = Tuple false Nothing -- unknown state. omit

-- | calculate local delta distances for all drag events
calcDragDelta :: (Vector3 -> Effect (Maybe Vector3)) -> Event SceneDragEvent -> Event Vector3
calcDragDelta toLocalF evt = mapAccum calcDelta e def
    where f d = map (mkNewDrag d) <$> toLocalF (d ^. _point)
          mkNewDrag d p = d # _point .~ p
          -- convert drag event to use local coordinate system
          e = compact (performEvent $ f <$> evt)
          zero = mkVec3 0.0 0.0 0.0

          calcDelta ne oldE | ne ^. _dragType == DragStart = Tuple ne zero
                            | otherwise                    = Tuple ne (ne ^. _point <-> oldE ^. _point)


newtype DraggableMesh a = DraggableMesh {
    mesh      :: Mesh a,
    dragged   :: Event SceneDragEvent,
    dragDelta :: Event Vector3
}

derive instance newtypeDraggableMesh :: Newtype (DraggableMesh a) _

dragEvtOn :: forall a. Mesh a -> Event SceneDragEvent
dragEvtOn m = makeEvent \k -> do
                  makeDraggable m k
                  pure (stopDraggable m)

-- helper function to convert a world Vector3 to the mesh's local coord
toLocal :: forall a. Object3D a -> Vector3 -> Effect (Maybe Vector3)
toLocal mesh v = if hasParent mesh
                 then Just <$> worldToLocal v (parent mesh)
                 else pure Nothing

mkDraggableMesh :: forall a geo mat. Geometry geo -> Material mat -> Effect (DraggableMesh a)
mkDraggableMesh geo mat = do
    mesh <- mkMesh geo mat

    let dragged = multicast $ dragEvtOn mesh
        dragDelta = multicast $ calcDragDelta (toLocal mesh) dragged
    
    pure $ DraggableMesh {
        mesh      : mesh,
        dragged   : dragged,
        dragDelta : dragDelta
    }

newtype TapDragMesh a = TapDragMesh {
    mesh      :: Mesh a,
    tapped    :: Event SceneTapEvent,
    dragged   :: Event SceneDragEvent,
    dragDelta :: Event Vector3
}

derive instance newtypeTapDragMesh :: Newtype (TapDragMesh a) _

mkTapDragMesh :: forall a geo mat. Geometry geo -> Material mat -> Effect (TapDragMesh a)
mkTapDragMesh geo mat = do
    m <- mkDraggableMesh geo mat
    pure $ TapDragMesh {
        mesh      : m ^. _mesh,
        tapped    : tapEvtOn $ m ^. _mesh,
        dragged   : m ^. _dragged,
        dragDelta : m ^. _dragDelta
    }
