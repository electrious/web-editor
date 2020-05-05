module Editor.Input where

import Prelude

import Control.Alt ((<|>))
import Data.Filterable (compact, filter)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Int (toNumber)
import Data.Lens (Lens', view, (.~), (^.))
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy(..))
import Data.Time.Duration (Milliseconds(..))
import FRP.Event (Event, fold, gate, makeEvent)
import FRP.Event.Extra (debounce, delay, multicast)
import Math (sqrt)
import Util (ffi)
import Web.DOM (Element)
import Web.DOM.Element (toEventTarget)
import Web.Event.Event (EventType, preventDefault)
import Web.Event.EventTarget (addEventListener, eventListener, removeEventListener)
import Web.Event.Internal.Types (EventTarget)
import Web.HTML.HTMLElement (DOMRect)
import Web.TouchEvent.EventTypes (touchend, touchmove, touchstart)
import Web.TouchEvent.Touch (clientY, pageX)
import Web.TouchEvent.TouchEvent (TouchEvent, touches)
import Web.TouchEvent.TouchEvent as TE
import Web.TouchEvent.TouchList (item)
import Web.UIEvent.MouseEvent (MouseEvent, shiftKey)
import Web.UIEvent.MouseEvent as ME
import Web.UIEvent.MouseEvent.EventTypes (mousedown, mousemove, mouseup)
import Web.UIEvent.WheelEvent as WE
import Web.UIEvent.WheelEvent.EventTypes (wheel)

-- | TapEvent
newtype TapEvent = TapEvent {
    tapX :: Number,
    tapY :: Number
}

derive instance newtypeTapEvent :: Newtype TapEvent _

_tapX :: Lens' TapEvent Number
_tapX = _Newtype <<< prop (SProxy :: SProxy "tapX")

_tapY :: Lens' TapEvent Number
_tapY = _Newtype <<< prop (SProxy :: SProxy "tapY")

-- | tap gesture recognizer for touches
tapped :: Event TapEvent -> Event TapEvent -> Event TapEvent
tapped start end = gate canBeTap tapCheck
    where s = const false <$> start
          e = const true <$> end
          canBeTap = s <|> e
          -- the touch should end in less than 0.32 seconds to be
          -- considered a tap.
          tapCheck = delay 320 start

-- | MouseMoveEvent encode the mouse position for MouseMove event
newtype MouseMoveEvent = MouseMoveEvent {
    mouseX :: Number,
    mouseY :: Number
}

derive instance newtypeMouseMoveEvent :: Newtype MouseMoveEvent _

_mouseX :: Lens' MouseMoveEvent Number
_mouseX = _Newtype <<< prop (SProxy :: SProxy "mouseX")

_mouseY :: Lens' MouseMoveEvent Number
_mouseY = _Newtype <<< prop (SProxy :: SProxy "mouseY")

mouseMoveEvent :: MouseEvent -> MouseMoveEvent
mouseMoveEvent e = MouseMoveEvent { mouseX: offsetX e, mouseY: offsetY e }

data DragType = DragStart
              | Drag
              | DragEnd

derive instance genericDragType :: Generic DragType _
derive instance eqDragType :: Eq DragType

instance showDragType :: Show DragType where
    show = genericShow

newtype DragEvent = DragEvent {
    dragType :: DragType,
    dragX    :: Number,
    dragY    :: Number,
    deltaX   :: Number,
    deltaY   :: Number
}

derive instance newtypeDragEvent :: Newtype DragEvent _

_dragType :: Lens' DragEvent DragType
_dragType = _Newtype <<< prop (SProxy :: SProxy "dragType")

_dragX :: Lens' DragEvent Number
_dragX = _Newtype <<< prop (SProxy :: SProxy "dragX")

_dragY :: Lens' DragEvent Number
_dragY = _Newtype <<< prop (SProxy :: SProxy "dragY")

_deltaX :: Lens' DragEvent Number
_deltaX = _Newtype <<< prop (SProxy :: SProxy "deltaX")

_deltaY :: Lens' DragEvent Number
_deltaY = _Newtype <<< prop (SProxy :: SProxy "deltaY")

isEnd :: DragEvent -> Boolean
isEnd (DragEvent e) = e.dragType == DragEnd

updateDragType :: DragType -> DragEvent -> DragEvent
updateDragType t e = e # _dragType .~ t

distance :: DragEvent -> DragEvent -> Number
distance e1 e2 = sqrt (dx * dx + dy * dy)
    where dx = e1 ^. _dragX - e2 ^. _dragX
          dy = e1 ^. _dragY - e2 ^. _dragY

-- wait for 2 seconds and see if there're new events
-- if not, make sure the last one is DragEnd
mkDragEndable :: Event DragEvent -> Event DragEvent
mkDragEndable evt = evt <|> compact (f <$> e)
    where e = debounce (Milliseconds 1500.0) evt
          f d = if d ^. _dragType /= DragEnd
                then Just $ d # _dragType .~ DragEnd
                              # _deltaX .~ 0.0
                              # _deltaY .~ 0.0
                else Nothing

newtype DragState = DragState {
    canDrag     :: Boolean,
    isDragging  :: Boolean,
    lastDragEvt :: Maybe DragEvent,
    curDragEvt  :: Maybe DragEvent
}

derive instance newtypeDragState :: Newtype DragState _

_canDrag :: Lens' DragState Boolean
_canDrag = _Newtype <<< prop (SProxy :: SProxy "canDrag")

_isDragging :: Lens' DragState Boolean
_isDragging = _Newtype <<< prop (SProxy :: SProxy "isDragging")

_lastDragEvt :: Lens' DragState (Maybe DragEvent)
_lastDragEvt = _Newtype <<< prop (SProxy :: SProxy "lastDragEvt")

_curDragEvt :: Lens' DragState (Maybe DragEvent)
_curDragEvt = _Newtype <<< prop (SProxy :: SProxy "curDragEvt")

defState :: DragState
defState = DragState { canDrag: false, isDragging: false, lastDragEvt: Nothing, curDragEvt: Nothing }

calcDelta :: DragEvent -> DragEvent -> DragEvent
calcDelta evt oEvt = evt # _deltaX .~ evt ^. _dragX - oEvt ^. _dragX
                         # _deltaY .~ evt ^. _dragY - oEvt ^. _dragY

processDrag :: DragEvent -> DragState -> DragState
processDrag evt st | evt ^. _dragType == DragStart = DragState { canDrag: true, isDragging: false, lastDragEvt: Just evt, curDragEvt: Nothing }
                   | evt ^. _dragType == Drag && not (st ^. _isDragging) && st ^. _canDrag =
                        if distance evt (fromMaybe evt $ st ^. _lastDragEvt) > 1.0
                        then let nEvt = evt # _dragType .~ DragStart
                             in st # _isDragging .~ true
                                   # _lastDragEvt .~ Just nEvt
                                   # _curDragEvt .~ Just nEvt
                        else st # _lastDragEvt .~ Just evt
                   | evt ^. _dragType == Drag && st ^. _isDragging =
                        let oEvt = fromMaybe evt $ st ^. _lastDragEvt
                            nEvt = calcDelta evt oEvt
                        in st # _lastDragEvt .~ Just nEvt
                              # _curDragEvt .~ Just nEvt
                   | evt ^. _dragType == DragEnd && st ^. _isDragging =
                        let oEvt = fromMaybe evt $ st ^. _lastDragEvt
                            nEvt = calcDelta evt oEvt
                        in st # _canDrag .~ false
                              # _isDragging .~ false
                              # _lastDragEvt .~ Just nEvt
                              # _curDragEvt .~ Just nEvt
                   | evt ^. _dragType == DragEnd && not (st ^. _isDragging) && st ^. _canDrag =
                        st # _canDrag .~ false
                           # _curDragEvt .~ Nothing
                           # _lastDragEvt .~ Just evt
                   | otherwise = st # _curDragEvt .~ Nothing
                                    # _lastDragEvt .~ Just evt

-- | drag gesture recognizer for both mouse and touch events
dragged :: Event TapEvent -> Event TapEvent -> Event TapEvent -> Event DragEvent
dragged start move end = compact $ view _curDragEvt <$> fold processDrag evts defState
      where mkDrag t e = DragEvent { dragType: t, dragX: e ^. _tapX, dragY: e ^. _tapY, deltaX: 0.0, deltaY: 0.0 }

            dragStart = mkDrag DragStart <$> start
            dragMove  = mkDrag Drag <$> move
            dragEnd   = mkDrag DragEnd <$> end
            evts = mkDragEndable $ dragStart <|> dragMove <|> dragEnd


newtype InputEvents = InputEvents {
    tapped       :: Event TapEvent,
    zoomed       :: Event WE.WheelEvent,
    dragged      :: Event DragEvent,
    shiftDragged :: Event DragEvent,
    mouseMove    :: Event MouseMoveEvent
}

derive instance newtypeInputEvents :: Newtype InputEvents _

_zoomed :: Lens' InputEvents (Event WE.WheelEvent)
_zoomed = _Newtype <<< prop (SProxy :: SProxy "zoomed")

_dragged :: Lens' InputEvents (Event DragEvent)
_dragged = _Newtype <<< prop (SProxy :: SProxy "dragged")

_shiftDragged :: Lens' InputEvents (Event DragEvent)
_shiftDragged = _Newtype <<< prop (SProxy :: SProxy "shiftDragged")

_mouseMove :: Lens' InputEvents (Event MouseMoveEvent)
_mouseMove = _Newtype <<< prop (SProxy :: SProxy "mouseMove")

mouseEvent :: EventType -> EventTarget -> Event ME.MouseEvent
mouseEvent t target = compact $ makeEvent \k -> do
    listener <- eventListener \e -> k (ME.fromEvent e)
    addEventListener t listener false target
    pure $ removeEventListener t listener false target

touchEvent :: EventType -> EventTarget -> Event TE.TouchEvent
touchEvent t target = compact $ makeEvent \k -> do
    listener <- eventListener \e -> k (TE.fromEvent e)
    addEventListener t listener false target
    pure $ removeEventListener t listener false target

wheelEvent :: EventTarget -> Event WE.WheelEvent
wheelEvent target = compact $ makeEvent \k -> do
    listener <- eventListener \e -> preventDefault e *> k (WE.fromEvent e)
    addEventListener wheel listener false target
    pure $ removeEventListener wheel listener false target

mouseTap :: MouseEvent -> TapEvent
mouseTap e = TapEvent { tapX: offsetX e, tapY: offsetY e }

offsetX :: MouseEvent -> Number
offsetX = ffi ["mouseEvt"] "mouseEvt.offsetX"

offsetY :: MouseEvent -> Number
offsetY = ffi ["mouseEvt"] "mouseEvt.offsetY"

touchTap :: Element -> TouchEvent -> Maybe TapEvent
touchTap elem e = tapT <$> item 0 (touches e)
    where tapT t = TapEvent { tapX: getX t, tapY: getY t }
          rect = getBoundingClientRect elem
          getX t = toNumber (pageX t) - rect.left
          getY t = toNumber (clientY t) - rect.top

foreign import getBoundingClientRect :: Element -> DOMRect

-- | setup the input system for an element.
setupInput :: Element -> InputEvents
setupInput elem =
    let target = toEventTarget elem
        -- get input events
        md = multicast $ mouseEvent mousedown target
        mm = multicast $ mouseEvent mousemove target
        mu = multicast $ mouseEvent mouseup target

        mouseStart = mouseTap <$> filter (not <<< shiftKey) md
        mouseMove  = mouseTap <$> filter (not <<< shiftKey) mm
        mouseEnd   = mouseTap <$> mu

        touchStart = compact $ touchTap elem <$> multicast (touchEvent touchstart target)
        touchMove  = compact $ touchTap elem <$> multicast (touchEvent touchmove target)
        touchEnd   = compact $ touchTap elem <$> multicast (touchEvent touchend target)

        wheelEvt = multicast $ wheelEvent target

        shiftStart = mouseTap <$> filter shiftKey md
        shiftMove  = mouseTap <$> filter shiftKey mm

        start = multicast $ mouseStart <|> touchStart
        move  = multicast $ mouseMove <|> touchMove
        end   = multicast $ mouseEnd <|> touchEnd

        drag      = dragged start move end
        shiftDrag = dragged shiftStart shiftMove mouseEnd

    in InputEvents {
        tapped       : multicast $ tapped start end,
        zoomed       : wheelEvt,
        dragged      : multicast drag,
        shiftDragged : multicast shiftDrag,
        mouseMove    : mouseMoveEvent <$> mm
    }