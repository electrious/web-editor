module UI.DraggableObject where

import Prelude hiding (add)

import Control.Alt ((<|>))
import Control.Monad.Reader (ask)
import Control.Monad.Writer (tell)
import Custom.Mesh (DraggableMesh, TapDragMesh, calcDragDelta, validateDrag)
import Data.Default (def)
import Data.Filterable (filter)
import Data.Lens (Lens', view, (^.), (.~))
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy(..))
import Data.Tuple (snd)
import Editor.Common.Lenses (_dragged, _name, _parent, _position, _tapped)
import Editor.Disposable (Disposee(..))
import Editor.SceneEvent (isDragEnd, isDragStart)
import Effect.Class (liftEffect)
import Effect.Unsafe (unsafePerformEffect)
import FRP.Dynamic (Dynamic, step)
import FRP.Event (Event, create, subscribe)
import FRP.Event.Extra (foldWithDef, multicast)
import Rendering.Node (Node, _renderOrder, _visible, dragMesh, leaf, node, tapDragMesh)
import Three.Core.Geometry (class IsGeometry, mkCircleGeometry)
import Three.Core.Material (MeshBasicMaterial, mkMeshBasicMaterial, setOpacity, setTransparent)
import Three.Core.Object3D (worldToLocal)
import Three.Math.Vector (Vector2, Vector3, mkVec3, vecX, vecY, (<+>))
import Unsafe.Coerce (unsafeCoerce)

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
    snd <$> tapDragMesh (def # _name     .~ "visible-obj"
                             # _position .~ posDyn
                             # _visible  .~ visDyn
                        ) g m leaf


invisibleCircle :: forall e. Dynamic Vector3 -> Dynamic Boolean -> Int -> Node e DraggableMesh
invisibleCircle posDyn visDyn rOrder = do
    geo <- liftEffect $ mkCircleGeometry 10.0 32
    snd <$> dragMesh (def # _name        .~ "invisible-circle"
                          # _position    .~ posDyn
                          # _visible     .~ visDyn
                          # _renderOrder .~ rOrder
                     ) geo invisibleMaterial leaf


-- | create a draggable object
createDraggableObject :: forall e geo. IsGeometry geo =>
                                        Event Boolean
                                        -> Vector2
                                        -> Maybe geo
                                        -> Maybe MeshBasicMaterial
                                        -> Node e DraggableObject
createDraggableObject active position customGeo customMat = node (def # _name .~ "drag-object") do
    { event : newPosEvt, push : pushNewPos } <- liftEffect create

    -- create the visible marker
    let defPos = mkVec3 (vecX position) (vecY position) 0.1
        posDyn = step defPos newPosEvt
        visDyn = step false active
    mesh <- visibleObj posDyn visDyn customGeo customMat

    { event : isDraggingEvt, push : pushIsDragging } <- liftEffect create
    -- create the invisible circle
    let vis2Dyn = step false isDraggingEvt
    invCircle <- invisibleCircle posDyn vis2Dyn 10

    let dragEvts = multicast $ mesh ^. _dragged <|> invCircle ^. _dragged
        evts     = multicast $ validateDrag dragEvts
        startEvt = filter isDragStart evts
        endEvt   = filter isDragEnd evts

        dragging = (const true <$> startEvt) <|> (const false <$> endEvt)

    d1 <- liftEffect $ subscribe dragging pushIsDragging

    parent <- view _parent <$> ask
    let toLocal v = Just <$> worldToLocal v parent
        delta = calcDragDelta toLocal evts

    -- function to calculate new position with delta
    let updatePos d lastPos = lastPos <+> zeroZ d
        zeroZ v = mkVec3 (vecX v) (vecY v) 0.0

        newPos = multicast $ foldWithDef updatePos delta defPos
    d2 <- liftEffect $ subscribe newPos pushNewPos

    tell $ Disposee $ d1 *> d2
    
    pure $ DraggableObject {
        tapped     : const unit <$> mesh ^. _tapped,
        position   : newPos,
        isDragging : multicast dragging
    }
