module Custom.Mesh where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Editor.Input (DragType(..))
import Editor.SceneEvent (SceneDragEvent, SceneTapEvent, makeDraggable, makeTappable, stopDraggable, stopTappable)
import Effect (Effect)
import FRP.Event (Event, makeEvent, mapAccum)
import Three.Core.Geometry (Geometry)
import Three.Core.Material (Material)
import Three.Core.Mesh (Mesh, mkMesh)
import Three.Core.Object3D (hasParent, parent, worldToLocal)
import Three.Math.Vector (Vector3, mkVec3, (<->))
import Util (performEvent, unwrap)

type TappableMesh a = {
    mesh   :: Mesh a,
    tapped :: Event SceneTapEvent
}

mkTappableMesh :: forall a geo mat. Geometry geo -> Material mat -> Effect (TappableMesh a)
mkTappableMesh geo mat = do
    mesh <- mkMesh geo mat
    let tapEvt = makeEvent \k -> do
                    makeTappable mesh k
                    pure (stopTappable mesh)
    pure {
        mesh   : mesh,
        tapped : tapEvt
    }


-- | process the drag events in the event stream to make sure all drag start
-- with dragStart and end with dragEnd
validateDrag :: Event SceneDragEvent -> Event SceneDragEvent
validateDrag evt = unwrap (mapAccum f evt false)
    where f e canDrag | e.type == DragStart = if canDrag
                                              then Tuple true Nothing  -- if there's a repeated drag start, omit it
                                              else Tuple true (Just e)
                      | canDrag && e.type == Drag = Tuple true (Just e)
                      | e.type == DragEnd = if canDrag
                                            then Tuple false (Just e) -- stop dragging, send the end event
                                            else Tuple false Nothing  -- already ended, omit the end event
                      | otherwise = Tuple false Nothing -- unknown state. omit

-- | calculate local delta distances for all drag events
calcDragDelta :: (Vector3 -> Effect (Maybe Vector3)) -> Event SceneDragEvent -> Event Vector3
calcDragDelta toLocal evt = mapAccum calcDelta evt def
    where f d = map (mkNewDrag d) <$> toLocal d.point
          mkNewDrag d p = { distance: d.distance, type: d.type, point: p }
          -- convert drag event to use local coordinate system
          e = unwrap (performEvent $ f <$> evt)
          zero = mkVec3 0.0 0.0 0.0
          def = { type: DragStart, distance: 0.0, point: zero }

          calcDelta e oldE | e.type == DragStart = Tuple e zero
                           | otherwise           = Tuple e (e.point <-> oldE.point)


type DraggableMesh a = {
    mesh      :: Mesh a,
    dragged   :: Event SceneDragEvent,
    dragDelta :: Event Vector3
}

mkDraggableMesh :: forall a geo mat. Geometry geo -> Material mat -> Effect (DraggableMesh a)
mkDraggableMesh geo mat = do
    mesh <- mkMesh geo mat

    let dragged = makeEvent \k -> do
                      makeDraggable mesh k
                      pure (stopDraggable mesh)
        toLocal v = if hasParent mesh
                    then Just <$> worldToLocal v (parent mesh)
                    else pure Nothing
        dragDelta = calcDragDelta toLocal dragged
    
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
    mesh <- mkMesh geo mat

    let tapEvt = makeEvent \k -> do
                    makeTappable mesh k
                    pure (stopTappable mesh)
        dragged = makeEvent \k -> do
                      makeDraggable mesh k
                      pure (stopDraggable mesh)
        toLocal v = if hasParent mesh
                    then Just <$> worldToLocal v (parent mesh)
                    else pure Nothing
        dragDelta = calcDragDelta toLocal dragged
    
    pure {
        mesh      : mesh,
        tapped    : tapEvt,
        dragged   : dragged,
        dragDelta : dragDelta
    }
