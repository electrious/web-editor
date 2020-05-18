module Editor.Input.Touch (touchRecognizer, TouchGestures(..), stopTouchScroll) where

import Prelude hiding (degree)

import Control.Alt ((<|>))
import Control.Plus (empty)
import Data.Compactable (compact)
import Data.Default (class Default, def)
import Data.Filterable (filter)
import Data.Int (toNumber)
import Data.Lens (Lens', (^.), (.~))
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy(..))
import Editor.Common.Lenses (_canDrag, _deltaX, _deltaY, _distance, _dragType, _isDragging, _x, _y)
import Editor.Input.Commoon (DragEvent, DragType(..), TapEvent, distance, dragDistance, middlePoint)
import Editor.Input.Drag (dragged)
import Effect (Effect)
import FRP.Event (Event, fold)
import Math (abs)
import Math.Angle (Angle, degree)
import Web.DOM (Element)
import Web.Event.Event (preventDefault)
import Web.HTML.HTMLElement (DOMRect)
import Web.TouchEvent (Touch, TouchEvent)
import Web.TouchEvent.Touch (clientY, pageX)
import Web.TouchEvent.TouchEvent (toEvent, touches)
import Web.TouchEvent.TouchList (item, length)


newtype DoubleTouchEvent = DoubleTouchEvent {
    dragType :: DragType,
    midPoint :: DragEvent,
    distance :: Number,
    rotation :: Angle
}

derive instance newtypeDoubleTouchEvent :: Newtype DoubleTouchEvent _
instance defaultDoubleTouchEvent :: Default DoubleTouchEvent where
    def = DoubleTouchEvent { dragType: DragStart, midPoint: def, distance: 0.0, rotation: degree 0.0 }

_midPoint :: forall t a r. Newtype t { midPoint :: a | r } => Lens' t a
_midPoint = _Newtype <<< prop (SProxy :: SProxy "midPoint")

singleTouchTap :: Element -> TouchEvent -> Maybe TapEvent
singleTouchTap elem = touchPosAt elem 0

tapToDrag :: DragType -> TapEvent -> DragEvent
tapToDrag t e = def # _dragType .~ t
                    # _x .~ e ^. _x
                    # _y .~ e ^. _y

doubleTouchTap :: Element -> DragType -> TouchEvent -> DoubleTouchEvent
doubleTouchTap elem t e = let tap1 = fromMaybe def $ tapToDrag t <$> touchPosAt elem 0 e
                              tap2 = fromMaybe def $ tapToDrag t <$> touchPosAt elem 1 e
                          in def # _dragType .~ t
                                 # _midPoint .~ middlePoint tap1 tap2
                                 # _distance .~ dragDistance tap1 tap2

touchPosAt :: Element -> Int -> TouchEvent -> Maybe TapEvent
touchPosAt elem idx e = touchPos elem <$> item idx (touches e)

touchPos :: Element -> Touch -> TapEvent
touchPos elem t = def # _x .~ toNumber (pageX t) - rect.left
                      # _y .~ toNumber (clientY t) - rect.top
    where rect = getBoundingClientRect elem

foreign import getBoundingClientRect :: Element -> DOMRect

stopTouchScroll :: TouchEvent -> Effect TouchEvent
stopTouchScroll e = preventDefault (toEvent e) *> pure e

newtype TouchGestures = TouchGestures {
    singleDrag :: Event DragEvent,
    doubleDrag :: Event DragEvent,
    pinch      :: Event Number,
    rotate     :: Event Angle
}

touchFingers :: TouchEvent -> Int
touchFingers = length <<< touches

singleTouch :: TouchEvent -> Boolean
singleTouch e = touchFingers e == 1

doubleTouch :: TouchEvent -> Boolean
doubleTouch e = touchFingers e == 2

data DoubleTouchType = Unknown
                     | Pinch
                     | DoubleDrag
                   --  | Rotate
derive instance eqDoubleTouchType :: Eq DoubleTouchType

newtype DoubleTouchState = DoubleTouchState {
    curType    :: DoubleTouchType,
    canDrag    :: Boolean,
    isDragging :: Boolean,
    lastDrag   :: Maybe DoubleTouchEvent,
    curDrag    :: Maybe DoubleTouchEvent
}

derive instance newtypeDoubleTouchState :: Newtype DoubleTouchState _
instance defaultDoubleTouchState :: Default DoubleTouchState where
    def = DoubleTouchState {
        curType    : Unknown,
        canDrag    : false,
        isDragging : false,
        lastDrag   : Nothing,
        curDrag    : Nothing
    }

_curType :: Lens' DoubleTouchState DoubleTouchType
_curType = _Newtype <<< prop (SProxy :: SProxy "curType")

_lastDrag :: Lens' DoubleTouchState (Maybe DoubleTouchEvent)
_lastDrag = _Newtype <<< prop (SProxy :: SProxy "lastDrag")

_curDrag :: Lens' DoubleTouchState (Maybe DoubleTouchEvent)
_curDrag = _Newtype <<< prop (SProxy :: SProxy "curDrag")

determineGesture :: DoubleTouchEvent -> DoubleTouchEvent -> DoubleTouchType
determineGesture oldE newE = let mvDst = distance (newE ^. _midPoint) (oldE ^. _midPoint)
                                 oldD = oldE ^. _distance
                                 newD = newE ^. _distance
                                 deltaD = abs (newD - oldD)
                             in if mvDst > 5.0
                                then DoubleDrag
                                else if deltaD > 1.0
                                    then Pinch
                                    else Unknown

processDoubleTouch :: DoubleTouchEvent -> DoubleTouchState -> DoubleTouchState
processDoubleTouch e s | e ^. _dragType == DragStart = def # _canDrag .~ true
                                                           # _curDrag .~ Just e
                       | e ^. _dragType == Drag && not (s ^. _isDragging) && s ^. _canDrag =
                            let nEvt = e # _dragType .~ DragStart
                                t = determineGesture nEvt (fromMaybe nEvt $ s ^. _lastDrag)
                            in s # _curType    .~ t
                                 # _isDragging .~ true
                                 # _lastDrag   .~ s ^. _curDrag
                                 # _curDrag    .~ Just nEvt
                       | e ^. _dragType == Drag && s ^. _isDragging && s ^. _curType == Unknown =
                            let nEvt = e # _dragType .~ DragStart
                                t = determineGesture nEvt (fromMaybe nEvt $ s ^. _lastDrag)
                            in s # _curType  .~ t
                                 # _lastDrag .~ s ^. _curDrag
                                 # _curDrag  .~ Just nEvt
                       | e ^. _dragType == Drag && s ^. _isDragging = s # _lastDrag .~ s ^. _curDrag
                                                                        # _curDrag  .~ Just e
                       | e ^. _dragType == DragEnd && s ^. _isDragging = s # _canDrag    .~ false
                                                                           # _isDragging .~ false
                                                                           # _lastDrag   .~ s ^. _curDrag
                                                                           # _curDrag    .~ Just e
                       | e ^. _dragType == DragEnd && not (s ^. _isDragging) && s ^. _canDrag =
                            s # _canDrag  .~ false
                              # _lastDrag .~ s ^. _curDrag
                              # _curDrag  .~ Just e
                       | otherwise = s # _lastDrag .~ s ^. _curDrag
                                       # _curDrag  .~ Just e


dragEvt :: DoubleTouchEvent -> DoubleTouchEvent -> DragEvent
dragEvt ce oe = d # _dragType .~ ce ^. _dragType
                  # _deltaX .~ d ^. _x - od ^. _x
                  # _deltaY .~ d ^. _y - od ^. _y
    where d = ce ^. _midPoint
          od = oe ^. _midPoint

getDoubleDragEvt :: DoubleTouchState -> Maybe DragEvent
getDoubleDragEvt s | s ^. _curType == DoubleDrag = dragEvt <$> s ^. _curDrag <*> s ^. _lastDrag
                   | otherwise                   = Nothing


pinchDelta :: DoubleTouchEvent -> DoubleTouchEvent -> Number
pinchDelta ce oe = oe ^. _distance - ce ^. _distance

getPinchDelta :: DoubleTouchState -> Maybe Number
getPinchDelta s | s ^. _curType == Pinch = pinchDelta <$> s ^. _curDrag <*> s ^. _lastDrag
                | otherwise              = Nothing

touchRecognizer :: Element -> Event TouchEvent -> Event TouchEvent -> Event TouchEvent -> TouchGestures
touchRecognizer elem start move end =
    let singleStart = compact $ singleTouchTap elem <$> filter singleTouch start
        singleMove  = compact $ singleTouchTap elem <$> filter singleTouch move
        singleEnd   = compact $ singleTouchTap elem <$> filter singleTouch end

        doubleStart = doubleTouchTap elem DragStart <$> filter doubleTouch start
        doubleMove  = doubleTouchTap elem Drag      <$> filter doubleTouch move
        doubleEnd   = doubleTouchTap elem DragEnd   <$> filter doubleTouch end

        doubleEvt = doubleStart <|> doubleMove <|> doubleEnd
        stEvt = fold processDoubleTouch doubleEvt def
        
    in TouchGestures {
        singleDrag : dragged singleStart singleMove singleEnd,
        doubleDrag : compact $ getDoubleDragEvt <$> stEvt,
        pinch      : compact $ getPinchDelta <$> stEvt,
        rotate     : empty
    }