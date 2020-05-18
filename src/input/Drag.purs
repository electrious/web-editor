module Editor.Input.Drag (dragged) where

import Prelude

import Control.Alt ((<|>))
import Data.Compactable (compact)
import Data.Default (class Default, def)
import Data.Lens (view, (.~), (^.))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (class Newtype)
import Data.Time.Duration (Milliseconds(..))
import Editor.Common.Lenses (_canDrag, _curDragEvt, _deltaX, _deltaY, _dragType, _isDragging, _lastDragEvt, _x, _y)
import Editor.Input.Commoon (DragEvent(..), DragType(..), TapEvent, distance)
import FRP.Event (Event, fold)
import FRP.Event.Extra (debounce)

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
instance defaultDragState :: Default DragState where
    def = DragState { canDrag: false, isDragging: false, lastDragEvt: Nothing, curDragEvt: Nothing }

calcDelta :: DragEvent -> DragEvent -> DragEvent
calcDelta evt oEvt = evt # _deltaX .~ evt ^. _x - oEvt ^. _x
                         # _deltaY .~ evt ^. _y - oEvt ^. _y

processDrag :: DragEvent -> DragState -> DragState
processDrag evt st | evt ^. _dragType == DragStart = def # _canDrag     .~ true
                                                         # _lastDragEvt .~ Just evt
                   | evt ^. _dragType == Drag && not (st ^. _isDragging) && st ^. _canDrag =
                        if distance evt (fromMaybe evt $ st ^. _lastDragEvt) > 1.0
                        then let nEvt = evt # _dragType .~ DragStart
                             in st # _isDragging  .~ true
                                   # _lastDragEvt .~ Just nEvt
                                   # _curDragEvt  .~ Just nEvt
                        else st # _lastDragEvt .~ Just evt
                   | evt ^. _dragType == Drag && st ^. _isDragging =
                        let oEvt = fromMaybe evt $ st ^. _lastDragEvt
                            nEvt = calcDelta evt oEvt
                        in st # _lastDragEvt .~ Just nEvt
                              # _curDragEvt  .~ Just nEvt
                   | evt ^. _dragType == DragEnd && st ^. _isDragging =
                        let oEvt = fromMaybe evt $ st ^. _lastDragEvt
                            nEvt = calcDelta evt oEvt
                        in st # _canDrag     .~ false
                              # _isDragging  .~ false
                              # _lastDragEvt .~ Just nEvt
                              # _curDragEvt  .~ Just nEvt
                   | evt ^. _dragType == DragEnd && not (st ^. _isDragging) && st ^. _canDrag =
                        st # _canDrag     .~ false
                           # _curDragEvt  .~ Nothing
                           # _lastDragEvt .~ Just evt
                   | otherwise = st # _curDragEvt  .~ Nothing
                                    # _lastDragEvt .~ Just evt

-- | drag gesture recognizer for both mouse and touch events
dragged :: Event TapEvent -> Event TapEvent -> Event TapEvent -> Event DragEvent
dragged start move end = compact $ view _curDragEvt <$> fold processDrag evts def
      where mkDrag t e = DragEvent { dragType: t, x: e ^. _x, y: e ^. _y, deltaX: 0.0, deltaY: 0.0 }

            dragStart = mkDrag DragStart <$> start
            dragMove  = mkDrag Drag <$> move
            dragEnd   = mkDrag DragEnd <$> end
            evts = mkDragEndable $ dragStart <|> dragMove <|> dragEnd
