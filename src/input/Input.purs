module Editor.Input where

import Prelude

import Control.Alt ((<|>))
import Data.Compactable (compact)
import Data.Filterable (filter)
import Data.Newtype (class Newtype)
import Editor.Input.Commoon (DragEvent, MouseMoveEvent, TapEvent, mouseMoveEvent, mouseTap)
import Editor.Input.Drag (dragged)
import Editor.Input.Touch (TouchGestures(..), stopTouchScroll, touchRecognizer)
import FRP.Event (Event, gate, makeEvent)
import FRP.Event.Extra (delay, multicast, performEvent)
import Web.DOM (Element)
import Web.DOM.Element (toEventTarget)
import Web.Event.Event (EventType, preventDefault)
import Web.Event.EventTarget (addEventListener, eventListener, removeEventListener)
import Web.Event.Internal.Types (EventTarget)
import Web.TouchEvent.TouchEvent as TE
import Web.TouchEvent.EventTypes (touchend, touchmove, touchstart)
import Web.UIEvent.MouseEvent (shiftKey)
import Web.UIEvent.MouseEvent as ME
import Web.UIEvent.MouseEvent.EventTypes (mousedown, mousemove, mouseup)
import Web.UIEvent.WheelEvent (deltaY)
import Web.UIEvent.WheelEvent as WE
import Web.UIEvent.WheelEvent.EventTypes (wheel)

-- | tap gesture recognizer for touches
tapped :: Event TapEvent -> Event TapEvent -> Event TapEvent
tapped start end = gate canBeTap tapCheck
    where s = const false <$> start
          e = const true <$> end
          canBeTap = s <|> e
          -- the touch should end in less than 0.32 seconds to be
          -- considered a tap.
          tapCheck = delay 320 start

newtype InputEvents = InputEvents {
    tapped       :: Event TapEvent,
    zoomed       :: Event Number,
    dragged      :: Event DragEvent,
    shiftDragged :: Event DragEvent,
    mouseMove    :: Event MouseMoveEvent
}

derive instance newtypeInputEvents :: Newtype InputEvents _

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

        touchStart = multicast (touchEvent touchstart target)
        touchMove  = multicast (performEvent $ stopTouchScroll <$> touchEvent touchmove target)
        touchEnd   = multicast (touchEvent touchend target)

        (TouchGestures touchRes) = touchRecognizer elem touchStart touchMove touchEnd
        wheelEvt = multicast $ deltaY <$> wheelEvent target

        shiftStart = mouseTap <$> filter shiftKey md
        shiftMove  = mouseTap <$> filter shiftKey mm

        start = multicast mouseStart
        move  = multicast mouseMove
        end   = multicast mouseEnd

        drag      = dragged start move end <|> touchRes.singleDrag
        shiftDrag = dragged shiftStart shiftMove mouseEnd <|> touchRes.doubleDrag

    in InputEvents {
        tapped       : multicast $ tapped start end,
        zoomed       : wheelEvt <|> touchRes.pinch,
        dragged      : multicast drag,
        shiftDragged : multicast shiftDrag,
        mouseMove    : mouseMoveEvent <$> mm
    }