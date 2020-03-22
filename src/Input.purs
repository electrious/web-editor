module Editor.Input where

import Prelude

import Control.Alt ((<|>))
import Data.Filterable (filter)
import Data.Generic.Rep (class Generic)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Data.Time.Duration (Milliseconds(..))
import FRP.Behavior (gate, sampleBy, sample_, step)
import FRP.Event (Event, fix, makeEvent, withLast)
import FRP.Event.Time (debounce)
import Math (sqrt)
import Util (delay, ffi, skip, unwrap)
import Web.DOM (Element)
import Web.DOM.Element (toEventTarget)
import Web.Event.Event (EventType)
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
          canBeTap = step false $ s <|> e
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
mkDragEndable evt = evt <|> unwrap (f <$> e)
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

-- | drag gesture recognizer for both mouse and touch events
dragged :: Event TapEvent -> Event TapEvent -> Event TapEvent -> Event DragEvent
dragged start move end = fix \possibleEnd ->
      let startDrag = const true <$> start
          endDrag   = const false <$> end

          posEnd = const false <$> possibleEnd
          -- we're only interested in move events between start and end
          touching = step false (startDrag <|> endDrag <|> posEnd)
          -- only move events in between touching is real move
          realMove = mkDrag Drag <$> gate touching move
          mkDrag t e = { dragType: t, dragX: e.tapX, dragY: e.tapY, deltaX: 0.0, deltaY: 0.0 }

          -- make sure user did move the mouse/touch
          checkDist (Just s) p = if distance s p >= 1.0 then Just p else Nothing
          checkDist Nothing p  = Nothing

          dstart = mkDrag DragStart <$> start
          startPos = step Nothing (Just <$> dstart)
          dragMove = unwrap (sampleBy checkDist startPos realMove)

          -- when user is actually dragging
          dragging = step false ((const false <$> start) <|>
                                 (const true <$> dragMove) <|>
                                 (const false <$> end))
          notDragging = not <$> dragging
          -- the drag start should be the first drag event attached with start position
          dragStart = unwrap (sample_ startPos (gate notDragging dragMove))

          -- calculate the new drag end event
          lastPos = step Nothing (Just <$> dragMove)
          lastDrag = unwrap (sample_ lastPos (gate dragging end))
          dragEnd = updateDragType DragEnd <$> lastDrag

          def = {dragType: DragStart, dragX: 0.0, dragY: 0.0, deltaX: 0.0, deltaY: 0.0}
          evts = dragStart <|> dragMove <|> dragEnd

          calcDelta { last, now } = case last of
                                    Just l -> { dragType: now.dragType,
                                                dragX: now.dragX,
                                                dragY: now.dragY,
                                                deltaX: now.dragX - l.dragX,
                                                deltaY: now.dragY - l.dragY
                                               }
                                    Nothing -> now
          resEvt = mkDragEndable $ skip 1 $ calcDelta <$> withLast evts
        in { input: filter isEnd resEvt, output: resEvt }


type InputEvents = {
    tapped       :: Event TapEvent,
    zoomed       :: Event WE.WheelEvent,
    dragged      :: Event DragEvent,
    shiftDragged :: Event DragEvent,
    mouseMove    :: Event MouseMoveEvent
}

mouseEvent :: EventType -> EventTarget -> Event ME.MouseEvent
mouseEvent t target = unwrap $ makeEvent \k -> do
    listener <- eventListener \e -> k (ME.fromEvent e)
    addEventListener t listener false target
    pure $ removeEventListener t listener false target

touchEvent :: EventType -> EventTarget -> Event TE.TouchEvent
touchEvent t target = unwrap $ makeEvent \k -> do
    listener <- eventListener \e -> k (TE.fromEvent e)
    addEventListener t listener false target
    pure $ removeEventListener t listener false target

wheelEvent :: EventTarget -> Event WE.WheelEvent
wheelEvent target = unwrap $ makeEvent \k -> do
    listener <- eventListener \e -> k (WE.fromEvent e)
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
        md = mouseEvent mousedown target
        mm = mouseEvent mousemove target
        mu = mouseEvent mouseup target

        mouseStart = mouseTap <$> filter (not <<< shiftKey) md
        mouseMove  = mouseTap <$> filter (not <<< shiftKey) mm
        mouseEnd   = mouseTap <$> mu

        touchStart = unwrap $ touchTap elem <$> touchEvent touchstart target
        touchMove = unwrap $ touchTap elem <$> touchEvent touchmove target
        touchEnd = unwrap $ touchTap elem <$> touchEvent touchend target

        wheelEvt = wheelEvent target

        shiftStart = mouseTap <$> filter shiftKey md
        shiftMove = mouseTap <$> filter shiftKey mm

        start = mouseStart <|> touchStart
        move = mouseMove <|> touchMove
        end = mouseEnd <|> touchEnd

        drag = dragged start move end
        shiftDrag = dragged shiftStart shiftMove mouseEnd

    in {
        tapped: tapped start end,
        zoomed: wheelEvt,
        dragged: drag,
        shiftDragged: shiftDrag,
        mouseMove: mouseMoveEvent <$> mm
    }