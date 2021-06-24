module UI.DraggableObject where

import Prelude hiding (add)

import Control.Alt ((<|>))
import Custom.Mesh (DraggableMesh, TapDragMesh, calcDragDelta, validateDrag)
import Data.Default (class Default, def)
import Data.Filterable (filter)
import Data.Lens (Lens', (.~), (^.))
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy(..))
import Editor.Common.Lenses (_dragged, _enabled, _isActive, _name, _position, _rotation, _tapped)
import Editor.SceneEvent (isDragEnd, isDragStart)
import Effect.Class (liftEffect)
import Effect.Unsafe (unsafePerformEffect)
import FRP.Dynamic (Dynamic, gateDyn, step)
import FRP.Event (Event, gate)
import FRP.Event.Extra (foldWithDef, multicast, performEvent)
import Rendering.Node (Node, Props, _raycastable, _renderOrder, _visible, dragMesh, fixNodeE, getParent, node, tapDragMesh)
import Three.Core.Geometry (class IsGeometry, CircleGeometry, mkCircleGeometry)
import Three.Core.Material (MeshBasicMaterial, doubleSide, mkMeshBasicMaterial, setOpacity, setSide, setTransparent)
import Three.Core.Object3D (localToParent, parentToLocal, worldToLocal)
import Three.Math.Euler (Euler)
import Three.Math.Vector (Vector3, mkVec3, vecX, vecY, vecZ, (<+>))
import Unsafe.Coerce (unsafeCoerce)

newtype DragObjCfg geo = DragObjCfg {
    isActive       :: Dynamic Boolean,
    enabled        :: Dynamic Boolean,
    position       :: Vector3,
    rotation       :: Euler,
    customGeo      :: Maybe geo,
    customMat      :: Maybe MeshBasicMaterial,
    validator      :: Vector3 -> Boolean,
    deltaTransform :: Maybe (Vector3 -> Vector3)
    }

derive instance newtypeDragObjCfg :: Newtype (DragObjCfg geo) _

instance defaultDragObjCfg :: Default (DragObjCfg geo) where
    def = DragObjCfg {
        isActive       : pure false,
        enabled        : pure true,
        position       : def,
        rotation       : def,
        customGeo      : Nothing,
        customMat      : Nothing,
        validator      : const true,
        deltaTransform : Nothing
        }

_customGeo :: forall t a r. Newtype t { customGeo :: a | r } => Lens' t a
_customGeo = _Newtype <<< prop (SProxy :: SProxy "customGeo")

_customMat :: forall t a r. Newtype t { customMat :: a | r } => Lens' t a
_customMat = _Newtype <<< prop (SProxy :: SProxy "customMat")

_validator :: forall t a r. Newtype t { validator :: a | r } => Lens' t a
_validator = _Newtype <<< prop (SProxy :: SProxy "validator")

_deltaTransform :: forall t a r. Newtype t { deltaTransform :: a | r } => Lens' t a
_deltaTransform = _Newtype <<< prop (SProxy :: SProxy "deltaTransform")

newtype DraggableObject = DraggableObject {
    tapped     :: Event Unit,
    position   :: Event Vector3,
    isDragging :: Event Boolean
}

derive instance newtypeDraggableEvent :: Newtype DraggableObject _

_isDragging :: forall t a r. Newtype t { isDragging :: a | r } => Lens' t a
_isDragging = _Newtype <<< prop (SProxy :: SProxy "isDragging")

-- | create the default material
defMaterial :: MeshBasicMaterial
defMaterial = unsafePerformEffect do
    mat <- mkMeshBasicMaterial 0xff2222
    setSide doubleSide mat
    pure mat

-- | invisible material for big circle under marker to easae dragging
invisibleMaterial :: MeshBasicMaterial
invisibleMaterial = unsafePerformEffect do
    mat <- mkMeshBasicMaterial 0
    setTransparent true mat
    setOpacity 0.001 mat
    pure mat


defVisCircGeo :: CircleGeometry
defVisCircGeo = unsafePerformEffect $ mkCircleGeometry 0.5 32

-- | create visible part of the object, user can specify custom geometry
-- and material
visibleObj :: forall e geo. IsGeometry geo => Props -> Maybe geo -> Maybe MeshBasicMaterial -> Node e TapDragMesh
visibleObj prop geo mat = tapDragMesh prop g m
    where g = fromMaybe (unsafeCoerce defVisCircGeo) geo
          m = fromMaybe defMaterial mat

invisCircGeo :: CircleGeometry
invisCircGeo = unsafePerformEffect $ mkCircleGeometry 10.0 32

invisibleCircle :: forall e. Props -> Node e DraggableMesh
invisibleCircle prop = dragMesh prop invisCircGeo invisibleMaterial


-- | increment z of a Vector3 by 0.1
incZ :: Vector3 -> Vector3
incZ p = mkVec3 (vecX p) (vecY p) (vecZ p + 0.1)


-- | decrement z of a Vector3 by 0.1
decZ :: Vector3 -> Vector3
decZ p = mkVec3 (vecX p) (vecY p) (vecZ p - 0.1)


-- | create a draggable object
createDraggableObject :: forall e geo. IsGeometry geo => DragObjCfg geo -> Node e DraggableObject
createDraggableObject cfg =
    node (def # _name     .~ "drag-object"
              # _rotation .~ pure (cfg ^. _rotation)
         ) $
        fixNodeE \newPosEvt ->
            fixNodeE \isDraggingEvt -> do
                parent <- getParent

                -- function to convert parameter Vector3 to local space
                -- and vice versa.
                -- all position vectors from user and back to user should be in parent's coordinate system
                let toLoc = map incZ <<< flip parentToLocal parent
                    toParent = flip localToParent parent <<< decZ
                
                -- all positions used below will be raised up a bit on Z axis
                position <- liftEffect $ toLoc (cfg ^. _position)
                let -- create the visible marker
                    posDyn = step position $ performEvent $ toLoc <$> newPosEvt
                mesh <- visibleObj (def # _name     .~ "visible-obj"
                                        # _position .~ posDyn
                                        # _visible  .~ cfg ^. _isActive
                                   ) (cfg ^. _customGeo) (cfg ^. _customMat)

                -- create the invisible circle
                let castable = step false isDraggingEvt
                invCircle <- invisibleCircle (def # _name        .~ "invisible-circle"
                                                  # _position    .~ posDyn
                                                  # _visible     .~ pure false
                                                  # _raycastable .~ castable
                                                  # _renderOrder .~ 10
                                             )

                let dragEvts = multicast $ mesh ^. _dragged <|> invCircle ^. _dragged
                    evts     = multicast $ gateDyn (cfg ^. _enabled) $ validateDrag dragEvts
                    startEvt = filter isDragStart evts
                    endEvt   = filter isDragEnd evts

                    dragging = multicast $ (const true <$> startEvt) <|> (const false <$> endEvt)

                    toLocal v = Just <$> worldToLocal v parent
                    delta = calcDragDelta toLocal evts

                    -- transform delta in parent coordinate system if deltaTransform provided
                    deltaEvt = case cfg ^. _deltaTransform of
                        Nothing -> delta
                        Just f  -> performEvent $ (toLoc <<< f <=< toParent) <$> delta

                    -- function to calculate new position with delta
                    updatePos d lastPos = lastPos <+> zeroZ d
                    zeroZ v = mkVec3 (vecX v) (vecY v) 0.0

                    -- validate and transform the new position with configured funtions
                    filtF  = cfg ^. _validator
                    newPos = multicast $ filter filtF $ performEvent $ toParent <$> foldWithDef updatePos deltaEvt position
                    
                    dragObj = DraggableObject {
                        tapped     : const unit <$> mesh ^. _tapped,
                        position   : gate dragging newPos,
                        isDragging : dragging
                    }
                pure { input : dragging, output: { input : newPos, output: dragObj } }
