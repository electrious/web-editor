module Editor.Input where

import Prelude

import Control.Alt ((<|>))
import Data.Filterable (compact, filter)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Time.Duration (Milliseconds(..))
import FRP.Event (Event, fold, gate, makeEvent)
import Math (sqrt)
import Util (delay, ffi, multicast, debounce)
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
type TapEvent = {
    tapX :: Number,
    tapY :: Number
}

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
type MouseMoveEvent = {
    mouseX :: Number,
    mouseY :: Number
}

mouseMoveEvent :: MouseEvent -> MouseMoveEvent
mouseMoveEvent e = { mouseX: offsetX e, mouseY: offsetY e }

data DragType = DragStart
              | Drag
              | DragEnd

derive instance genericDragType :: Generic DragType _
derive instance eqDragType :: Eq DragType

instance showDragType :: Show DragType where
    show = genericShow

type DragEvent = {
    dragType :: DragType,
    dragX    :: Number,
    dragY    :: Number,
    deltaX   :: Number,
    deltaY   :: Number
}

isEnd :: DragEvent -> Boolean
isEnd e = e.dragType == DragEnd

updateDragType :: DragType -> DragEvent -> DragEvent
updateDragType t e = {
    dragType : t,
    dragX    : e.dragX,
    dragY    : e.dragY,
    deltaX   : e.deltaX,
    deltaY   : e.deltaY
}

distance :: DragEvent -> DragEvent -> Number
distance e1 e2 = sqrt (dx * dx + dy * dy)
    where dx = e1.dragX - e2.dragX
          dy = e1.dragY - e2.dragY

-- wait for 2 seconds and see if there're new events
-- if not, make sure the last one is DragEnd
mkDragEndable :: Event DragEvent -> Event DragEvent
mkDragEndable evt = evt <|> compact (f <$> e)
    where e = debounce (Milliseconds 1500.0) evt
          f d = if d.dragType /= DragEnd
                then Just {
                    dragType: DragEnd,
                    dragX: d.dragX,
                    dragY: d.dragY,
                    deltaX: 0.0,
                    deltaY: 0.0
                }
                else Nothing

type DragState = {
    canDrag     :: Boolean,
    isDragging  :: Boolean,
    lastDragEvt :: Maybe DragEvent,
    curDragEvt  :: Maybe DragEvent
}

defState :: DragState
defState = { canDrag: false, isDragging: false, lastDragEvt: Nothing, curDragEvt: Nothing }

calcDelta :: DragEvent -> DragEvent -> DragEvent
calcDelta evt oEvt = evt { deltaX = evt.dragX - oEvt.dragX, deltaY = evt.dragY - oEvt.dragY }

processDrag :: DragEvent -> DragState -> DragState
processDrag evt st | evt.dragType == DragStart = { canDrag: true, isDragging: false, lastDragEvt: Just evt, curDragEvt: Nothing }
                   | evt.dragType == Drag && not st.isDragging && st.canDrag = if distance evt (fromMaybe evt st.lastDragEvt) > 1.0
                                                                               then let nEvt = evt { dragType = DragStart }
                                                                                    in st { isDragging = true, lastDragEvt = Just nEvt, curDragEvt = Just nEvt }
                                                                               else st { lastDragEvt = Just evt }
                   | evt.dragType == Drag && st.isDragging = let oEvt = fromMaybe evt st.lastDragEvt
                                                                 nEvt = calcDelta evt oEvt
                                                             in st { lastDragEvt = Just nEvt, curDragEvt = Just nEvt }
                   | evt.dragType == DragEnd = let oEvt = fromMaybe evt st.lastDragEvt
                                                   nEvt = calcDelta evt oEvt
                                               in st { canDrag = false, isDragging = false, lastDragEvt = Just nEvt, curDragEvt = Just nEvt }
                   | otherwise = st { curDragEvt = Nothing, lastDragEvt = Just evt }

-- | drag gesture recognizer for both mouse and touch events
dragged :: Event TapEvent -> Event TapEvent -> Event TapEvent -> Event DragEvent
dragged start move end = multicast $ compact $ _.curDragEvt <$> fold processDrag evts defState
      where mkDrag t e = { dragType: t, dragX: e.tapX, dragY: e.tapY, deltaX: 0.0, deltaY: 0.0 }

            dragStart = mkDrag DragStart <$> start
            dragMove  = mkDrag Drag <$> move
            dragEnd   = mkDrag DragEnd <$> end
            evts = mkDragEndable $ dragStart <|> dragMove <|> dragEnd


type InputEvents = {
    tapped       :: Event TapEvent,
    zoomed       :: Event WE.WheelEvent,
    dragged      :: Event DragEvent,
    shiftDragged :: Event DragEvent,
    mouseMove    :: Event MouseMoveEvent
}

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
mouseTap e = { tapX: offsetX e, tapY: offsetY e }

offsetX :: MouseEvent -> Number
offsetX = ffi ["mouseEvt"] "mouseEvt.offsetX"

offsetY :: MouseEvent -> Number
offsetY = ffi ["mouseEvt"] "mouseEvt.offsetY"

touchTap :: Element -> TouchEvent -> Maybe TapEvent
touchTap elem e = tapT <$> item 0 (touches e)
    where tapT t = { tapX: getX t, tapY: getY t }
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
        touchMove = compact $ touchTap elem <$> multicast (touchEvent touchmove target)
        touchEnd = compact $ touchTap elem <$> multicast (touchEvent touchend target)

        wheelEvt = multicast $ wheelEvent target

        shiftStart = mouseTap <$> filter shiftKey md
        shiftMove = mouseTap <$> filter shiftKey mm

        start = multicast $ mouseStart <|> touchStart
        move = multicast $ mouseMove <|> touchMove
        end = multicast $ mouseEnd <|> touchEnd

        drag = dragged start move end
        shiftDrag = dragged shiftStart shiftMove mouseEnd

    in {
        tapped: multicast $ tapped start end,
        zoomed: wheelEvt,
        dragged: multicast drag,
        shiftDragged: multicast shiftDrag,
        mouseMove: mouseMoveEvent <$> mm
    }