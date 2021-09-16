module Editor.SceneEvent where

import Prelude

import Control.Alt ((<|>))
import Data.Array (filter, head)
import Data.Compactable (compact)
import Data.Default (class Default)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.Int (toNumber)
import Data.Lens (Lens', (.~), (^.))
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Type.Proxy (Proxy(..))
import Data.Time.Duration (Milliseconds(..))
import Data.Traversable (sequence_, traverse)
import Editor.Common.Lenses (_dragType, _dragged, _height, _mouseMove, _tapped, _width, _x, _y)
import Editor.Disposable (class Disposable)
import Editor.Input (InputEvents)
import Editor.Input.Commoon (DragEvent, DragType(..))
import Effect (Effect)
import FRP.Dynamic (Dynamic, sampleDyn)
import FRP.Event (Event, subscribe)
import FRP.Event.Extra (debounce, multicast, performEvent)
import Three.Core.Camera (class IsCamera)
import Three.Core.Face3 (Face3)
import Three.Core.Object3D (class IsObject3D, Object3D, disableLayer, enableLayer, toObject3D)
import Three.Core.Raycaster (Intersection, distance, face, intersectObject, mkRaycaster, object, point, setFromCamera)
import Three.Math.Vector (Vector2, Vector3, mkVec2, mkVec3)

newtype Size = Size {
    width  :: Int,
    height :: Int
}

derive instance newtypeSize :: Newtype Size _
derive instance genericSize :: Generic Size _
instance showSize :: Show Size where
    show = genericShow

size :: Int -> Int -> Size
size w h = Size { width: w, height: h }

-- | tap events sent to 3D objects
newtype SceneTapEvent = SceneTapEvent {
    distance    :: Number,
    point       :: Vector3,
    domPosition :: Vector2
}

derive instance newtypeSceneTapEvent :: Newtype SceneTapEvent _

-- | mousemove events sent to 3D object
newtype SceneMouseMoveEvent = SceneMouseMoveEvent {
    distance    :: Number,
    point       :: Vector3, -- position in world coord
    face        :: Face3, -- face with world normal
    domPosition :: Vector2 -- original mouse event position
}

derive instance newtypeSceneMouseMoveEvent :: Newtype SceneMouseMoveEvent _

-- | drag events sent to 3D objects
newtype SceneDragEvent = SceneDragEvent {
    dragType :: DragType,
    distance :: Number,
    point    :: Vector3
}

derive instance newtypeSceneDragEvent :: Newtype SceneDragEvent _
instance defaultSceneDragEvent :: Default SceneDragEvent where
    def = SceneDragEvent { dragType: DragStart, distance: 0.0, point: mkVec3 0.0 0.0 0.0 }

isDragStart :: forall t r. Newtype t { dragType :: DragType | r } => t -> Boolean
isDragStart e = e ^. _dragType == DragStart

isDrag :: forall t r. Newtype t { dragType :: DragType | r } => t -> Boolean
isDrag e = e ^. _dragType == Drag

isDragEnd :: forall t r. Newtype t { dragType :: DragType | r } => t -> Boolean
isDragEnd e = e ^. _dragType == DragEnd

-- | add end event to SceneDragEvent stream if there's no input for a while
-- and no end event.
mkDragEndable :: Event SceneDragEvent -> Event SceneDragEvent
mkDragEndable evt = evt <|> compact (f <$> e)
    where e = debounce (Milliseconds 2000.0) evt
          f d = if d ^. _dragType /= DragEnd
                then Just $ d # _dragType .~ DragEnd
                else Nothing

-- | Convert an Object3D to be tappable by attaching a callback
-- function for tap events.
foreign import makeTappableJS :: Object3D -> (SceneTapEvent -> Effect Unit) -> Effect Unit
foreign import stopTappableJS :: Object3D -> Effect Unit
foreign import isTappableJS   :: Object3D -> Boolean
foreign import sendTapEventJS :: Object3D -> SceneTapEvent -> Effect Unit

makeTappable :: forall a. IsObject3D a => a -> (SceneTapEvent -> Effect Unit) -> Effect Unit
makeTappable o = makeTappableJS (toObject3D o)

stopTappable :: forall a. IsObject3D a => a -> Effect Unit
stopTappable = stopTappableJS <<< toObject3D

isTappable :: forall a. IsObject3D a => a -> Boolean
isTappable = isTappableJS <<< toObject3D

sendTapEvent :: forall a. IsObject3D a => a -> SceneTapEvent -> Effect Unit
sendTapEvent o = sendTapEventJS (toObject3D o)

-- | Convert an Object3D to be MouseMovable by attaching a callback
-- function for mouseMove events.
foreign import makeMouseMoveJS      :: Object3D -> (SceneMouseMoveEvent -> Effect Unit) -> Effect Unit
foreign import stopMouseMoveJS      :: Object3D -> Effect Unit
foreign import isMouseMoveJS        :: Object3D -> Boolean
foreign import sendMouseMoveEventJS :: Object3D -> SceneMouseMoveEvent -> Effect Unit

makeMouseMove :: forall a. IsObject3D a => a -> (SceneMouseMoveEvent -> Effect Unit) -> Effect Unit
makeMouseMove o = makeMouseMoveJS (toObject3D o)

stopMouseMove :: forall a. IsObject3D a => a -> Effect Unit
stopMouseMove = stopMouseMoveJS <<< toObject3D

isMouseMove :: forall a. IsObject3D a => a -> Boolean
isMouseMove = isMouseMoveJS <<< toObject3D

sendMouseMoveEvent :: forall a. IsObject3D a => a -> SceneMouseMoveEvent -> Effect Unit
sendMouseMoveEvent o = sendMouseMoveEventJS (toObject3D o)

-- | Convert an Object3D to be Draggable by attaching a callback
-- function for drag events.
foreign import makeDraggableJS :: Object3D -> (SceneDragEvent -> Effect Unit) -> Effect Unit
foreign import stopDraggableJS :: Object3D -> Effect Unit
foreign import isDraggableJS   :: Object3D -> Boolean
foreign import sendDragEventJS :: Object3D -> SceneDragEvent -> Effect Unit

makeDraggable :: forall a. IsObject3D a => a -> (SceneDragEvent -> Effect Unit) -> Effect Unit
makeDraggable o = makeDraggableJS (toObject3D o)

stopDraggable :: forall a. IsObject3D a => a -> Effect Unit
stopDraggable = stopDraggableJS <<< toObject3D

isDraggable :: forall a. IsObject3D a => a -> Boolean
isDraggable = isDraggableJS <<< toObject3D

sendDragEvent :: forall a. IsObject3D a => a -> SceneDragEvent -> Effect Unit
sendDragEvent o = sendDragEventJS (toObject3D o)

-- | convert mouse/touch position to values between -1 and 1
calcPosition :: forall t r. Newtype t { x :: Number, y :: Number | r} => Size -> t -> Vector2
calcPosition s pos = mkVec2 x y
    where x = (pos ^. _x / toNumber (s ^. _width)) * 2.0 - 1.0
          y = - (pos ^. _y / toNumber (s ^. _height)) * 2.0 + 1.0

-- | Find first object in the intersections array that is Tappable and
-- send the event to it.
processTapObjects :: Vector2 -> Array Intersection -> Effect Unit
processTapObjects domPos objs = void $ traverse doTap target
    where target = head $ filter (isTappable <<< object) objs
          doTap o = sendTapEvent (object o) $ SceneTapEvent {
              distance    : distance o,
              point       : point o,
              domPosition : domPos
          }

processMouseOverObjects :: Vector2 -> Array Intersection -> Effect Unit
processMouseOverObjects domPos objs = void $ traverse doMove target
    where target = head $ filter (isMouseMove <<< object) objs
          doMove o = sendMouseMoveEvent (object o) $ SceneMouseMoveEvent {
              distance    : distance o,
              point       : point o,
              face        : face o,
              domPosition : domPos
          }

processDragObjects :: DragEvent -> Array Intersection -> Effect (Maybe DragEvent)
processDragObjects e objs = traverse doDrag target *> pure (f target)
    where target = head $ filter (isDraggable <<< object) objs
          doDrag o = sendDragEvent (object o) $ SceneDragEvent {
              dragType : e ^. _dragType,
              distance : distance o,
              point    : point o
            }
          f (Just _) = Nothing
          f Nothing  = Just e


-- enable raycasting on a object by enabling the default layer 0
enableRaycasting :: forall o. IsObject3D o => o -> Effect Unit
enableRaycasting o = enableLayer 0 o *> disableLayer 1 o

-- disable raycasting by disable layer 0 and enable layer 1, so it can still be
-- visible to the camera, which is working on both layer 0 and 1.
disableRaycasting :: forall o. IsObject3D o => o -> Effect Unit
disableRaycasting o = disableLayer 0 o *> enableLayer 1 o

setRaycastable :: forall o. IsObject3D o => o -> Boolean -> Effect Unit
setRaycastable o true  = enableRaycasting o
setRaycastable o false = disableRaycasting o


newtype RaycastSetup = RaycastSetup {
    dragEvent  :: Event DragEvent,
    disposable :: Effect Unit
}

derive instance newtypeRaycastSetup :: Newtype RaycastSetup _

instance disposableRaycastSetup :: Disposable RaycastSetup where
    dispose (RaycastSetup { disposable }) = disposable

_dragEvent :: Lens' RaycastSetup (Event DragEvent)
_dragEvent = _Newtype <<< prop (Proxy :: Proxy "dragEvent")

-- | setup all raycasting needed to process user inputs and send
-- them to the corresponding 3D object in the scene
setupRaycasting :: forall a b. IsCamera a => IsObject3D b => a -> b -> InputEvents -> Dynamic Size -> Effect RaycastSetup
setupRaycasting camera scene input sizeDyn = do
    raycaster <- mkRaycaster
    
    let doRaycast tp = do
            setFromCamera raycaster tp camera
            intersectObject raycaster scene true
        
        raycastTap sz e = do
            let domPos = mkVec2 (e ^. _x) (e ^. _y)
            res <- doRaycast (calcPosition sz e)
            processTapObjects domPos res

        raycastMouse sz e = do
            let domPos = mkVec2 (e ^. _x) (e ^. _y)
            res <- doRaycast (calcPosition sz e)
            processMouseOverObjects domPos res
        
        raycastDrag sz e = do
            res <- doRaycast (calcPosition sz e)
            processDragObjects e res
    
    let e1 = performEvent $ sampleDyn sizeDyn (flip raycastTap <$> input ^. _tapped)
        e2 = performEvent $ sampleDyn sizeDyn (flip raycastMouse <$> input ^. _mouseMove)
        unraycastedDrag = compact $ performEvent $ sampleDyn sizeDyn (flip raycastDrag <$> input ^. _dragged)

        f _ = pure unit
    
    d1 <- subscribe e1 f
    d2 <- subscribe e2 f

    pure $ RaycastSetup {
        dragEvent  : multicast unraycastedDrag,
        disposable : sequence_ [d1, d2]
    }
