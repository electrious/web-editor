module UI.DraggableObject where

import Prelude hiding (add)

import Control.Alt ((<|>))
import Custom.Mesh (DraggableMesh, TapDragMesh, calcDragDelta, mkDraggableMesh, mkTapDragMesh, validateDrag)
import Data.Filterable (filter)
import Data.Foldable (sequence_)
import Data.Maybe (Maybe(..), fromMaybe)
import Editor.SceneEvent (isDragEnd, isDragStart)
import Effect (Effect)
import Effect.Unsafe (unsafePerformEffect)
import FRP.Event (Event, subscribe)
import Three.Core.Geometry (Geometry, mkCircleGeometry)
import Three.Core.Material (Material, mkMeshBasicMaterial, setOpacity, setTransparent)
import Three.Core.Object3D (Object3D, add, hasParent, mkObject3D, parent, setName, setPosition, setRenderOrder, setVisible, worldToLocal)
import Three.Math.Vector (Vector2, Vector3, mkVec3, vecX, vecY, (<+>))
import Unsafe.Coerce (unsafeCoerce)
import Util (foldWithDef, multicast)

type DraggableObject a = {
    object     :: Object3D a,
    tapped     :: Event Int,
    position   :: Event Vector3,
    isDragging :: Event Boolean,
    disposable :: Effect Unit
}

-- | create the default material
defMaterial :: forall a. Material a
defMaterial = unsafeCoerce $ unsafePerformEffect (mkMeshBasicMaterial 0xff2222)

-- | invisible material for big circle under marker to easae dragging
invisibleMaterial :: forall a. Material a
invisibleMaterial = unsafeCoerce $ unsafePerformEffect do
    mat <- mkMeshBasicMaterial 0
    setTransparent true mat
    setOpacity 0.001 mat
    pure mat

-- | create visible part of the object, user can specify custom geometry
-- and material
createVisibleObject :: forall a geo mat. Maybe (Geometry geo) -> Maybe (Material mat) -> Effect (TapDragMesh a)
createVisibleObject geo mat = do
    cm <- mkCircleGeometry 0.5 32
    let g = fromMaybe (unsafeCoerce cm) geo
        m = fromMaybe defMaterial mat
    mkTapDragMesh g m


createInvisibleCircle :: forall a. Effect (DraggableMesh a)
createInvisibleCircle = do
    geo <- mkCircleGeometry 10.0 32
    mkDraggableMesh geo invisibleMaterial


-- | create a draggable object
createDraggableObject :: forall a geo mat. Event Boolean
                                        -> Int
                                        -> Vector2
                                        -> Maybe (Geometry geo)
                                        -> Maybe (Material mat)
                                        -> Effect (DraggableObject a)
createDraggableObject active index position customGeo customMat = do
    dragObj <- mkObject3D
    setName "drag-object" dragObj

    -- create the visible marker
    mesh <- createVisibleObject customGeo customMat

    let defPosition = mkVec3 (vecX position) (vecY position) 0.1
    setPosition defPosition mesh.mesh
    setVisible false mesh.mesh  -- invisible by default
    
    add mesh.mesh dragObj

    -- create the invisible circle
    invCircle <- createInvisibleCircle
    setPosition defPosition invCircle.mesh
    setVisible false invCircle.mesh
    setRenderOrder 10 invCircle.mesh
    add invCircle.mesh dragObj

    disp1 <- subscribe active (flip setVisible mesh.mesh)

    let dragEvts = multicast $ mesh.dragged <|> invCircle.dragged
        evts = multicast $ validateDrag dragEvts
        startEvt = filter isDragStart evts
        endEvt = filter isDragEnd evts

        dragging = (const true <$> startEvt) <|> (const false <$> endEvt)
    
    disp2 <- subscribe dragging (flip setVisible invCircle.mesh)

    let toLocal v = if hasParent dragObj
                    then Just <$> worldToLocal v (parent dragObj)
                    else pure Nothing
        delta = calcDragDelta toLocal evts

    -- function to calculate new position with delta
    let updatePos d lastPos = lastPos <+> zeroZ d
        zeroZ v = mkVec3 (vecX v) (vecY v) 0.0

        newPos = multicast $ foldWithDef updatePos delta defPosition
    
    disp3 <- subscribe newPos \p -> do
                setPosition p mesh.mesh
                setPosition p invCircle.mesh

    pure {
        object: dragObj,
        tapped: const index <$> mesh.tapped,
        position: newPos,
        isDragging: multicast dragging,
        disposable: sequence_ [disp1, disp2, disp3]
    }
