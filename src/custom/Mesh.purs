module Custom.Mesh where

import Prelude

import Data.Compactable (compact)
import Data.Default (def)
import Data.Lens (view, (.~), (^.))
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Tuple (Tuple(..))
import Editor.Common.Lenses (_dragDelta, _dragType, _dragged, _mesh, _point)
import Editor.Input.Commoon (DragType(..))
import Editor.SceneEvent (SceneDragEvent, SceneTapEvent, makeDraggable, makeTappable, stopDraggable, stopTappable)
import Effect (Effect)
import FRP.Event (Event, makeEvent, mapAccum)
import FRP.Event.Extra (multicast, performEvent)
import Three.Core.Geometry (class IsGeometry)
import Three.Core.Material (class IsMaterial)
import Three.Core.Mesh (Mesh, mkMesh)
import Three.Core.Object3D (class IsObject3D, Object3D, hasParent, parent, toObject3D, worldToLocal)
import Three.Math.Vector (Vector3, mkVec3, (<->))

newtype TappableMesh = TappableMesh {
    mesh   :: Mesh,
    tapped :: Event SceneTapEvent
}

derive instance newtypeTappableMesh :: Newtype TappableMesh _
instance isObject3DTappableMesh :: IsObject3D TappableMesh where
    toObject3D = toObject3D <<< view _mesh

tapEvtOn :: Mesh -> Event SceneTapEvent
tapEvtOn m = makeEvent \k -> do
                makeTappable m k
                pure (stopTappable m)

mkTappableMesh :: forall geo mat. IsGeometry geo => IsMaterial mat => geo -> mat -> Effect TappableMesh
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

          calcDelta ne oldE | ne ^. _dragType == DragStart = Tuple ne def
                            | otherwise                    = Tuple ne (ne ^. _point <-> oldE ^. _point)


newtype DraggableMesh = DraggableMesh {
    mesh      :: Mesh,
    dragged   :: Event SceneDragEvent,
    dragDelta :: Event Vector3
}

derive instance newtypeDraggableMesh :: Newtype DraggableMesh _
instance isObject3DDraggableMesh :: IsObject3D DraggableMesh where
    toObject3D = toObject3D <<< view _mesh

dragEvtOn :: Mesh -> Event SceneDragEvent
dragEvtOn m = makeEvent \k -> do
                  makeDraggable m k
                  pure (stopDraggable m)

-- helper function to convert a world Vector3 to the mesh's local coord
toLocal :: forall a. IsObject3D a => a -> Vector3 -> Effect (Maybe Vector3)
toLocal mesh v = if hasParent mesh
                 then Just <$> worldToLocal v (parent mesh :: Object3D)
                 else pure Nothing

mkDraggableMesh :: forall geo mat. IsGeometry geo => IsMaterial mat => geo -> mat -> Effect DraggableMesh
mkDraggableMesh geo mat = do
    mesh <- mkMesh geo mat

    let dragged = multicast $ dragEvtOn mesh
        dragDelta = multicast $ calcDragDelta (toLocal mesh) dragged
    
    pure $ DraggableMesh {
        mesh      : mesh,
        dragged   : dragged,
        dragDelta : dragDelta
    }

newtype TapDragMesh = TapDragMesh {
    mesh      :: Mesh,
    tapped    :: Event SceneTapEvent,
    dragged   :: Event SceneDragEvent,
    dragDelta :: Event Vector3
}

derive instance newtypeTapDragMesh :: Newtype TapDragMesh _
instance toObject3DTapDragMesh :: IsObject3D TapDragMesh where
    toObject3D = toObject3D <<< view _mesh

mkTapDragMesh :: forall geo mat. IsGeometry geo => IsMaterial mat => geo -> mat -> Effect TapDragMesh
mkTapDragMesh geo mat = do
    m <- mkDraggableMesh geo mat
    pure $ TapDragMesh {
        mesh      : m ^. _mesh,
        tapped    : tapEvtOn $ m ^. _mesh,
        dragged   : m ^. _dragged,
        dragDelta : m ^. _dragDelta
    }
