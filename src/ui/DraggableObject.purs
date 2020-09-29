module UI.DraggableObject where

import Prelude hiding (add)

import Control.Alt ((<|>))
import Custom.Mesh (DraggableMesh, TapDragMesh, calcDragDelta, mkDraggableMesh, mkTapDragMesh, validateDrag)
import Data.Filterable (filter)
import Data.Foldable (sequence_)
import Data.Lens (Lens', view, (^.))
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy(..))
import Editor.Common.Lenses (_disposable, _dragged, _object, _tapped)
import Editor.Disposable (class Disposable)
import Editor.SceneEvent (isDragEnd, isDragStart)
import Effect (Effect)
import Effect.Unsafe (unsafePerformEffect)
import FRP.Event (Event, subscribe)
import FRP.Event.Extra (foldWithDef, multicast)
import Three.Core.Geometry (class IsGeometry, mkCircleGeometry)
import Three.Core.Material (MeshBasicMaterial, mkMeshBasicMaterial, setOpacity, setTransparent)
import Three.Core.Object3D (class IsObject3D, Object3D, add, hasParent, mkObject3D, parent, setName, setPosition, setRenderOrder, setVisible, worldToLocal)
import Three.Math.Vector (Vector2, Vector3, mkVec3, vecX, vecY, (<+>))
import Unsafe.Coerce (unsafeCoerce)

newtype DraggableObject = DraggableObject {
    object     :: Object3D,
    tapped     :: Event Int,
    position   :: Event Vector3,
    isDragging :: Event Boolean,
    disposable :: Effect Unit
}

derive instance newtypeDraggableEvent :: Newtype DraggableObject _
instance isObject3DDraggableObject :: IsObject3D DraggableObject where
    toObject3D = view _object
instance disposableDraggableObject :: Disposable DraggableObject where
    dispose d = d ^. _disposable

_position :: Lens' DraggableObject (Event Vector3)
_position = _Newtype <<< prop (SProxy :: SProxy "position")

_isDragging :: Lens' DraggableObject (Event Boolean)
_isDragging = _Newtype <<< prop (SProxy :: SProxy "isDragging")

-- | create the default material
defMaterial :: MeshBasicMaterial
defMaterial = unsafeCoerce $ unsafePerformEffect (mkMeshBasicMaterial 0xff2222)

-- | invisible material for big circle under marker to easae dragging
invisibleMaterial :: MeshBasicMaterial
invisibleMaterial = unsafeCoerce $ unsafePerformEffect do
    mat <- mkMeshBasicMaterial 0
    setTransparent true mat
    setOpacity 0.001 mat
    pure mat

-- | create visible part of the object, user can specify custom geometry
-- and material
createVisibleObject :: forall geo. IsGeometry geo => Maybe geo -> Maybe MeshBasicMaterial -> Effect TapDragMesh
createVisibleObject geo mat = do
    cm <- mkCircleGeometry 0.5 32
    let g = fromMaybe (unsafeCoerce cm) geo
        m = fromMaybe defMaterial mat
    mkTapDragMesh g m


createInvisibleCircle :: Effect DraggableMesh
createInvisibleCircle = do
    geo <- mkCircleGeometry 10.0 32
    mkDraggableMesh geo invisibleMaterial


-- | create a draggable object
createDraggableObject :: forall geo. IsGeometry geo =>
                                        Event Boolean
                                        -> Int
                                        -> Vector2
                                        -> Maybe geo
                                        -> Maybe MeshBasicMaterial
                                        -> Effect DraggableObject
createDraggableObject active index position customGeo customMat = do
    dragObj <- mkObject3D
    setName "drag-object" dragObj

    -- create the visible marker
    mesh <- createVisibleObject customGeo customMat

    let defPosition = mkVec3 (vecX position) (vecY position) 0.1
    setPosition defPosition mesh
    setVisible false mesh  -- invisible by default
    
    add mesh dragObj

    -- create the invisible circle
    invCircle <- createInvisibleCircle
    setPosition defPosition invCircle
    setVisible false invCircle
    setRenderOrder 10 invCircle
    add invCircle dragObj

    disp1 <- subscribe active (flip setVisible mesh)

    let dragEvts = multicast $ mesh ^. _dragged <|> invCircle ^. _dragged
        evts = multicast $ validateDrag dragEvts
        startEvt = filter isDragStart evts
        endEvt = filter isDragEnd evts

        dragging = (const true <$> startEvt) <|> (const false <$> endEvt)
    
    disp2 <- subscribe dragging (flip setVisible invCircle)

    let toLocal v = if hasParent dragObj
                    then Just <$> worldToLocal v (parent dragObj :: Object3D)
                    else pure Nothing
        delta = calcDragDelta toLocal evts

    -- function to calculate new position with delta
    let updatePos d lastPos = lastPos <+> zeroZ d
        zeroZ v = mkVec3 (vecX v) (vecY v) 0.0

        newPos = multicast $ foldWithDef updatePos delta defPosition
    
    disp3 <- subscribe newPos \p -> do
                setPosition p mesh
                setPosition p invCircle

    pure $ DraggableObject {
        object     : dragObj,
        tapped     : const index <$> mesh ^. _tapped,
        position   : newPos,
        isDragging : multicast dragging,
        disposable : sequence_ [disp1, disp2, disp3]
    }
