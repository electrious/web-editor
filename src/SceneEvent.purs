module Editor.SceneEvent where

import Prelude

import Control.Alt ((<|>))
import Control.Apply (lift2)
import Data.Array (filter, head)
import Data.Compactable (compact)
import Data.Int (toNumber)
import Data.Lens (Lens', (.~), (^.))
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy(..))
import Data.Time.Duration (Milliseconds(..))
import Data.Traversable (sequence_, traverse)
import Editor.Input (DragEvent, DragType(..), InputEvents, MouseMoveEvent, TapEvent, _dragType, _dragX, _dragY, _dragged, _mouseMove, _mouseX, _mouseY, _tapX, _tapY, _tapped)
import Effect (Effect)
import FRP.Event (Event, subscribe)
import FRP.Event.Extra (debounce, multicast, performEvent)
import Three.Core.Camera (Camera)
import Three.Core.Face3 (Face3)
import Three.Core.Object3D (Object3D)
import Three.Core.Raycaster (Intersection, distance, face, intersectObject, mkRaycaster, object, point, setFromCamera)
import Three.Math.Vector (Vector2, Vector3, mkVec2, vecX, vecY)
import Util (ffi, fpi)

newtype Size = Size {
    width  :: Int,
    height :: Int
}

derive instance newtypeSize :: Newtype Size _

_width :: Lens' Size Int
_width = _Newtype <<< prop (SProxy :: SProxy "width")

_height :: Lens' Size Int
_height = _Newtype <<< prop (SProxy :: SProxy "height")

-- | tap events sent to 3D objects
newtype SceneTapEvent = SceneTapEvent {
    distance    :: Number,
    point       :: Vector3,
    domPosition :: Vector2
}

derive instance newtypeSceneTapEvent :: Newtype SceneTapEvent _

_tapDistance :: Lens' SceneTapEvent Number
_tapDistance = _Newtype <<< prop (SProxy :: SProxy "distance")

_tapPoint :: Lens' SceneTapEvent Vector3
_tapPoint = _Newtype <<< prop (SProxy :: SProxy "point")

_tapDomPosition :: Lens' SceneTapEvent Vector2
_tapDomPosition = _Newtype <<< prop (SProxy :: SProxy "domPosition")

-- | mousemove events sent to 3D object
newtype SceneMouseMoveEvent = SceneMouseMoveEvent {
    distance    :: Number,
    point       :: Vector3, -- position in local coord of the target object
    face        :: Face3, -- face with local normal of the target object
    domPosition :: Vector2 -- original mouse event position
}

derive instance newtypeSceneMouseMoveEvent :: Newtype SceneMouseMoveEvent _

_mouseDistance :: Lens' SceneMouseMoveEvent Number
_mouseDistance = _Newtype <<< prop (SProxy :: SProxy "distance")

_mousePoint :: Lens' SceneMouseMoveEvent Vector3
_mousePoint = _Newtype <<< prop (SProxy :: SProxy "point")

_mouseDomPosition :: Lens' SceneMouseMoveEvent Vector2
_mouseDomPosition = _Newtype <<< prop (SProxy :: SProxy "domPosition")

_face :: Lens' SceneMouseMoveEvent Face3
_face = _Newtype <<< prop (SProxy :: SProxy "face")

-- | drag events sent to 3D objects
newtype SceneDragEvent = SceneDragEvent {
    type     :: DragType,
    distance :: Number,
    point    :: Vector3
}

derive instance newtypeSceneDragEvent :: Newtype SceneDragEvent _

_type :: Lens' SceneDragEvent DragType
_type = _Newtype <<< prop (SProxy :: SProxy "type")

_dragDistance :: Lens' SceneDragEvent Number
_dragDistance = _Newtype <<< prop (SProxy :: SProxy "distance")

_dragPoint :: Lens' SceneDragEvent Vector3
_dragPoint = _Newtype <<< prop (SProxy :: SProxy "point")

isDragStart :: SceneDragEvent -> Boolean
isDragStart e = e ^. _type == DragStart

isDrag :: SceneDragEvent -> Boolean
isDrag e = e ^. _type == Drag

isDragEnd :: SceneDragEvent -> Boolean
isDragEnd e = e ^. _type == DragEnd

-- | add end event to SceneDragEvent stream if there's no input for a while
-- and no end event.
mkDragEndable :: Event SceneDragEvent -> Event SceneDragEvent
mkDragEndable evt = evt <|> compact (f <$> e)
    where e = debounce (Milliseconds 2000.0) evt
          f d = if d ^. _type /= DragEnd
                then Just $ d # _type .~ DragEnd
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
calcPosition :: Size -> Vector2 -> Vector2
calcPosition s pos = mkVec2 x y
    where x = (vecX pos / toNumber (s ^. _width)) * 2.0 - 1.0
          y = - (vecY pos / toNumber (s ^. _height)) * 2.0 + 1.0

-- | convert a TapEvent to position used for raycasting
tapPosition :: Size -> TapEvent -> Vector2
tapPosition s e = calcPosition s $ mkVec2 (e ^. _tapX) (e ^. _tapY)

-- | convert a MouseMoveEvent to position used for raycasting
mousePosition :: Size -> MouseMoveEvent -> Vector2
mousePosition s e = calcPosition s $ mkVec2 (e ^. _mouseX) (e ^. _mouseY)

-- | convert a DragEvent to position used for raycasting
dragPosition :: Size -> DragEvent -> Vector2
dragPosition s e = calcPosition s $ mkVec2 (e ^. _dragX) (e ^. _dragY)

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
              type: e ^. _dragType,
              distance: distance o,
              point: point o
            }
          f (Just _) = Nothing
          f Nothing  = Just e

newtype RaycastSetup = RaycastSetup {
    dragEvent :: Event DragEvent,
    dispose   :: Effect Unit
}

derive instance newtypeRaycastSetup :: Newtype RaycastSetup _

_dragEvent :: Lens' RaycastSetup (Event DragEvent)
_dragEvent = _Newtype <<< prop (SProxy :: SProxy "dragEvent")

_dispose :: Lens' RaycastSetup (Effect Unit)
_dispose = _Newtype <<< prop (SProxy :: SProxy "dispose")

-- | setup all raycasting needed to process user inputs and send
-- them to the corresponding 3D object in the scene
setupRaycasting :: forall a b. Camera a -> Object3D b -> InputEvents -> Event Size -> Effect RaycastSetup
setupRaycasting camera scene input size = do
    raycaster <- mkRaycaster
    
    let doRaycast tp = do
            setFromCamera raycaster tp camera
            intersectObject raycaster scene true
        
        raycastTap sz e = do
            let domPos = mkVec2 (e ^. _tapX) (e ^. _tapY)
            res <- doRaycast (tapPosition sz e)
            processTapObjects domPos res

        raycastMouse sz e = do
            let domPos = mkVec2 (e ^. _mouseX) (e ^. _mouseY)
            res <- doRaycast (mousePosition sz e)
            processMouseOverObjects domPos res
        
        raycastDrag sz e = do
            res <- doRaycast (dragPosition sz e)
            processDragObjects e res
    
    let e1 = performEvent $ lift2 raycastTap size (input ^. _tapped)
        e2 = performEvent $ lift2 raycastMouse size (input ^. _mouseMove)
        unraycastedDrag = compact $ performEvent $ lift2 raycastDrag size (input ^. _dragged)

        f _ = pure unit
    
    d1 <- subscribe e1 f
    d2 <- subscribe e2 f

    pure $ RaycastSetup {
        dragEvent: multicast unraycastedDrag,
        dispose: sequence_ [d1, d2]
    }
