module UI.DraggableObject where

import Prelude hiding (add)

import Control.Alt ((<|>))
import Control.Monad.Reader (ask)
import Control.Plus (empty)
import Custom.Mesh (DraggableMesh, TapDragMesh, calcDragDelta, validateDrag)
import Data.Default (class Default, def)
import Data.Filterable (filter)
import Data.Lens (Lens', view, (.~), (^.))
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy(..))
import Editor.Common.Lenses (_dragged, _name, _parent, _position, _tapped)
import Editor.SceneEvent (isDragEnd, isDragStart)
import Effect.Class (liftEffect)
import Effect.Unsafe (unsafePerformEffect)
import FRP.Dynamic (Dynamic, step)
import FRP.Event (Event)
import FRP.Event.Extra (foldWithDef, multicast)
import Model.Hardware.PanelModel (_isActive)
import Rendering.Node (Node, _renderOrder, _visible, dragMesh, fixNodeE, getEnv, node, tapDragMesh)
import Three.Core.Geometry (class IsGeometry, mkCircleGeometry)
import Three.Core.Material (MeshBasicMaterial, mkMeshBasicMaterial, setOpacity, setTransparent)
import Three.Core.Object3D (worldToLocal)
import Three.Math.Vector (Vector2, Vector3, mkVec3, vecX, vecY, (<+>))
import Unsafe.Coerce (unsafeCoerce)

newtype DragObjCfg geo = DragObjCfg {
    isActive       :: Dynamic Boolean,
    position       :: Vector2,
    customGeo      :: Maybe geo,
    customMat      :: Maybe MeshBasicMaterial,
    validator      :: Vector3 -> Boolean,
    deltaTransform :: Vector3 -> Vector3
    }

derive instance newtypeDragObjCfg :: Newtype (DragObjCfg geo) _

instance defaultDragObjCfg :: Default (DragObjCfg geo) where
    def = DragObjCfg {
        isActive       : step false empty,
        position       : def,
        customGeo      : Nothing,
        customMat      : Nothing,
        validator      : const true,
        deltaTransform : identity
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
defMaterial = unsafePerformEffect (mkMeshBasicMaterial 0xff2222)

-- | invisible material for big circle under marker to easae dragging
invisibleMaterial :: MeshBasicMaterial
invisibleMaterial = unsafePerformEffect do
    mat <- mkMeshBasicMaterial 0
    setTransparent true mat
    setOpacity 0.001 mat
    pure mat

-- | create visible part of the object, user can specify custom geometry
-- and material
visibleObj :: forall e geo. IsGeometry geo => Dynamic Vector3 -> Dynamic Boolean -> Maybe geo -> Maybe MeshBasicMaterial -> Node e TapDragMesh
visibleObj posDyn visDyn geo mat = do
    cm <- liftEffect $ mkCircleGeometry 0.5 32
    let g = fromMaybe (unsafeCoerce cm) geo
        m = fromMaybe defMaterial mat
    tapDragMesh (def # _name     .~ "visible-obj"
                     # _position .~ posDyn
                     # _visible  .~ visDyn
                ) g m


invisibleCircle :: forall e. Dynamic Vector3 -> Dynamic Boolean -> Int -> Node e DraggableMesh
invisibleCircle posDyn visDyn rOrder = do
    geo <- liftEffect $ mkCircleGeometry 10.0 32
    dragMesh (def # _name        .~ "invisible-circle"
                  # _position    .~ posDyn
                  # _visible     .~ visDyn
                  # _renderOrder .~ rOrder
             ) geo invisibleMaterial


-- | create a draggable object
createDraggableObject :: forall geo. IsGeometry geo => Node (DragObjCfg geo) DraggableObject
createDraggableObject =
    node (def # _name .~ "drag-object") $
        fixNodeE \newPosEvt ->
            fixNodeE \isDraggingEvt -> do
                cfg <- getEnv
                let position  = cfg ^. _position
                    visDyn    = cfg ^. _isActive
                    customGeo = cfg ^. _customGeo
                    customMat = cfg ^. _customMat
                    
                    -- create the visible marker
                    defPos = mkVec3 (vecX position) (vecY position) 0.1
                    posDyn = step defPos newPosEvt
                mesh <- visibleObj posDyn visDyn customGeo customMat

                -- create the invisible circle
                let vis2Dyn = step false isDraggingEvt
                invCircle <- invisibleCircle posDyn vis2Dyn 10

                let dragEvts = multicast $ mesh ^. _dragged <|> invCircle ^. _dragged
                    evts     = multicast $ validateDrag dragEvts
                    startEvt = filter isDragStart evts
                    endEvt   = filter isDragEnd evts

                    dragging = multicast $ (const true <$> startEvt) <|> (const false <$> endEvt)

                parent <- view _parent <$> ask
                let toLocal v = Just <$> worldToLocal v parent
                    delta = calcDragDelta toLocal evts

                    -- function to calculate new position with delta
                    updatePos d lastPos = lastPos <+> zeroZ d
                    zeroZ v = mkVec3 (vecX v) (vecY v) 0.0

                    -- validate and transform the new position with configured funtions
                    filtF  = cfg ^. _validator
                    transF = cfg ^. _deltaTransform
                    
                    newPos = multicast $ filter filtF $ foldWithDef updatePos (transF <$> delta) defPos
                    
                    dragObj = DraggableObject {
                        tapped     : const unit <$> mesh ^. _tapped,
                        position   : newPos,
                        isDragging : dragging
                    }
                pure { input : dragging, output: { input : newPos, output: dragObj } }
