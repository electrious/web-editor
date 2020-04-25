module Custom.Mesh where

import Prelude

import Data.Compactable (compact)
import Data.Lens ((^.), (.~))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Editor.Input (DragType(..))
import Editor.SceneEvent (SceneDragEvent(..), SceneTapEvent, _dragPoint, _type, makeDraggable, makeTappable, stopDraggable, stopTappable)
import Effect (Effect)
import FRP.Event (Event, makeEvent, mapAccum)
import FRP.Event.Extra (multicast, performEvent)
import Three.Core.Geometry (Geometry)
import Three.Core.Material (Material)
import Three.Core.Mesh (Mesh, mkMesh)
import Three.Core.Object3D (Object3D, hasParent, parent, worldToLocal)
import Three.Math.Vector (Vector3, mkVec3, (<->))

type TappableMesh a = {
    mesh   :: Mesh a,
    tapped :: Event SceneTapEvent
}

tapEvtOn :: forall a. Mesh a -> Event SceneTapEvent
tapEvtOn m = makeEvent \k -> do
                makeTappable m k
                pure (stopTappable m)

mkTappableMesh :: forall a geo mat. Geometry geo -> Material mat -> Effect (TappableMesh a)
mkTappableMesh geo mat = do
    mesh <- mkMesh geo mat
    pure {
        mesh   : mesh,
        tapped : tapEvtOn mesh
    }


-- | process the drag events in the event stream to make sure all drag start
-- with dragStart and end with dragEnd
validateDrag :: Event SceneDragEvent -> Event SceneDragEvent
validateDrag evt = compact (mapAccum f evt false)
    where f e canDrag | e ^. _type == DragStart = if canDrag
                                                  then Tuple true Nothing  -- if there's a repeated drag start, omit it
                                                  else Tuple true (Just e)
                      | canDrag && e ^. _type == Drag = Tuple true (Just e)
                      | e ^. _type == DragEnd = if canDrag
                                                then Tuple false (Just e) -- stop dragging, send the end event
                                                else Tuple false Nothing  -- already ended, omit the end event
                      | otherwise = Tuple false Nothing -- unknown state. omit

-- | calculate local delta distances for all drag events
calcDragDelta :: (Vector3 -> Effect (Maybe Vector3)) -> Event SceneDragEvent -> Event Vector3
calcDragDelta toLocalF evt = mapAccum calcDelta e def
    where f d = map (mkNewDrag d) <$> toLocalF (d ^. _dragPoint)
          mkNewDrag d p = d # _dragPoint .~ p
          -- convert drag event to use local coordinate system
          e = compact (performEvent $ f <$> evt)
          zero = mkVec3 0.0 0.0 0.0
          def = SceneDragEvent { type: DragStart, distance: 0.0, point: zero }

          calcDelta ne oldE | ne ^. _type == DragStart = Tuple ne zero
                            | otherwise                = Tuple ne (ne ^. _dragPoint <-> oldE ^. _dragPoint)


type DraggableMesh a = {
    mesh      :: Mesh a,
    dragged   :: Event SceneDragEvent,
    dragDelta :: Event Vector3
}

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
    
    pure {
        mesh      : mesh,
        dragged   : dragged,
        dragDelta : dragDelta
    }

type TapDragMesh a = {
    mesh      :: Mesh a,
    tapped    :: Event SceneTapEvent,
    dragged   :: Event SceneDragEvent,
    dragDelta :: Event Vector3
}

mkTapDragMesh :: forall a geo mat. Geometry geo -> Material mat -> Effect (TapDragMesh a)
mkTapDragMesh geo mat = do
    m <- mkDraggableMesh geo mat
    pure {
        mesh      : m.mesh,
        tapped    : tapEvtOn m.mesh,
        dragged   : m.dragged,
        dragDelta : m.dragDelta
    }
