module Editor.SceneEvent where

import Prelude

import Control.Alt ((<|>))
import Data.Array (filter, head)
import Data.Compactable (compact)
import Data.Default (class Default)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Int (toNumber)
import Data.Lens (Lens', (.~), (^.))
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy(..))
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
import Three.Core.Camera (Camera)
import Three.Core.Face3 (Face3)
import Three.Core.Object3D (Object3D)
import Three.Core.Raycaster (Intersection, distance, face, intersectObject, mkRaycaster, object, point, setFromCamera)
import Three.Math.Vector (Vector2, Vector3, mkVec2, mkVec3)
import Util (ffi, fpi)

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
    point       :: Vector3, -- position in local coord of the target object
    face        :: Face3, -- face with local normal of the target object
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

isDragStart :: SceneDragEvent -> Boolean
isDragStart e = e ^. _dragType == DragStart

isDrag :: SceneDragEvent -> Boolean
isDrag e = e ^. _dragType == Drag

isDragEnd :: SceneDragEvent -> Boolean
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
makeTappable :: forall a. Object3D a -> (SceneTapEvent -> Effect Unit) -> Effect Unit
makeTappable = fpi ["obj", "cb", ""] "obj.tapped = cb"

stopTappable :: forall a. Object3D a -> Effect Unit
stopTappable = fpi ["obj", ""] "obj.tapped = undefined"

isTappable :: forall a. Object3D a -> Boolean
isTappable = ffi ["obj"] "obj.tapped !== undefined"

sendTapEvent :: forall a. Object3D a -> SceneTapEvent -> Effect Unit
sendTapEvent = fpi ["obj", "evt", ""] "obj.tapped(evt)()"


-- | Convert an Object3D to be MouseMovable by attaching a callback
-- function for mouseMove events.
makeMouseMove :: forall a. Object3D a -> (SceneMouseMoveEvent -> Effect Unit) -> Effect Unit
makeMouseMove = fpi ["obj", "cb", ""] "obj.mouseMove = cb"

stopMouseMove :: forall a. Object3D a -> Effect Unit
stopMouseMove = fpi ["obj", ""] "obj.mouseMove = undefined"

isMouseMove :: forall a. Object3D a -> Boolean
isMouseMove = ffi ["obj"] "obj.mouseMove !== undefined"

sendMouseMoveEvent :: forall a. Object3D a -> SceneMouseMoveEvent -> Effect Unit
sendMouseMoveEvent = fpi ["obj", "evt", ""] "obj.mouseMove(evt)()"

-- | Convert an Object3D to be Draggable by attaching a callback
-- function for drag events.
makeDraggable :: forall a. Object3D a -> (SceneDragEvent -> Effect Unit) -> Effect Unit
makeDraggable = fpi ["obj", "cb", ""] "obj.dragged = cb"

stopDraggable :: forall a. Object3D a -> Effect Unit
stopDraggable = fpi ["obj", ""] "obj.dragged = undefined"

isDraggable :: forall a. Object3D a -> Boolean
isDraggable = ffi ["obj"] "obj.dragged !== undefined"

sendDragEvent :: forall a. Object3D a -> SceneDragEvent -> Effect Unit
sendDragEvent = fpi ["obj", "evt", ""] "obj.dragged(evt)()"

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

newtype RaycastSetup = RaycastSetup {
    dragEvent  :: Event DragEvent,
    disposable :: Effect Unit
}

derive instance newtypeRaycastSetup :: Newtype RaycastSetup _

instance disposableRaycastSetup :: Disposable RaycastSetup where
    dispose (RaycastSetup { disposable }) = disposable

_dragEvent :: Lens' RaycastSetup (Event DragEvent)
_dragEvent = _Newtype <<< prop (SProxy :: SProxy "dragEvent")

-- | setup all raycasting needed to process user inputs and send
-- them to the corresponding 3D object in the scene
setupRaycasting :: forall a b. Camera a -> Object3D b -> InputEvents -> Dynamic Size -> Effect RaycastSetup
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
