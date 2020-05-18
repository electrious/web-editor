module Editor.Input.Commoon where

import Prelude

import Data.Default (class Default, def)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Lens ((^.), (.~))
import Data.Newtype (class Newtype)
import Editor.Common.Lenses (_dragType, _x, _y)
import Math (sqrt)
import Util (ffi)
import Web.UIEvent.MouseEvent (MouseEvent)


-- | TapEvent
newtype TapEvent = TapEvent {
    x :: Number,
    y :: Number
}

derive instance newtypeTapEvent :: Newtype TapEvent _
instance defaultTapEvent :: Default TapEvent where
    def = TapEvent { x: 0.0, y: 0.0 }

-- | MouseMoveEvent encode the mouse position for MouseMove event
newtype MouseMoveEvent = MouseMoveEvent {
    x :: Number,
    y :: Number
}

derive instance newtypeMouseMoveEvent :: Newtype MouseMoveEvent _

mouseMoveEvent :: MouseEvent -> MouseMoveEvent
mouseMoveEvent e = MouseMoveEvent { x: offsetX e, y: offsetY e }

mouseTap :: MouseEvent -> TapEvent
mouseTap e = def # _x .~ offsetX e
                 # _y .~ offsetY e

offsetX :: MouseEvent -> Number
offsetX = ffi ["mouseEvt"] "mouseEvt.offsetX"

offsetY :: MouseEvent -> Number
offsetY = ffi ["mouseEvt"] "mouseEvt.offsetY"

data DragType = DragStart
              | Drag
              | DragEnd

derive instance genericDragType :: Generic DragType _
derive instance eqDragType :: Eq DragType

instance showDragType :: Show DragType where
    show = genericShow

newtype DragEvent = DragEvent {
    dragType :: DragType,
    x        :: Number,
    y        :: Number,
    deltaX   :: Number,
    deltaY   :: Number
}

derive instance newtypeDragEvent :: Newtype DragEvent _
instance defDragEvent :: Default DragEvent where
    def = DragEvent {
        dragType : DragStart,
        x        : 0.0,
        y        : 0.0,
        deltaX   : 0.0,
        deltaY   : 0.0
    }

isEnd :: DragEvent -> Boolean
isEnd (DragEvent e) = e.dragType == DragEnd

updateDragType :: DragType -> DragEvent -> DragEvent
updateDragType t e = e # _dragType .~ t

distance :: DragEvent -> DragEvent -> Number
distance e1 e2 = sqrt (dx * dx + dy * dy)
    where dx = e1 ^. _x - e2 ^. _x
          dy = e1 ^. _y - e2 ^. _y

middlePoint :: DragEvent -> DragEvent -> DragEvent
middlePoint t1 t2 = def # _x .~ mid (t1 ^. _x) (t2 ^. _x)
                        # _y .~ mid (t1 ^. _y) (t2 ^. _y)
    where mid v1 v2 = (v1 + v2) / 2.0

dragDistance :: DragEvent -> DragEvent -> Number
dragDistance t1 t2 = sqrt $ dx * dx + dy * dy
    where dx = t1 ^. _x - t2 ^. _x
          dy = t1 ^. _y - t2 ^. _y
