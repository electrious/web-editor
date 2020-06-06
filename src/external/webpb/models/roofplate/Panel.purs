module WebPB.Models.Roofplate.Panel where

import Prelude

import Effect (Effect)
import Util (ffi, fpi)
import WebPB.Class (class IsPBArrayComp)
import WebPB.UUID (PBUUID)

foreign import data OrientationPB :: Type
foreign import orientationInvalid   :: OrientationPB
foreign import orientationLandscape :: OrientationPB
foreign import orientationPortrait  :: OrientationPB

foreign import data AlignmentPB :: Type
foreign import alignmentInvalid :: AlignmentPB
foreign import alignmentGrid    :: AlignmentPB
foreign import alignmentBrick   :: AlignmentPB

foreign import data PanelPB :: Type
foreign import mkPanelPB :: Effect PanelPB

instance isPBArrayCompPanelPB :: IsPBArrayComp PanelPB

getId :: PanelPB -> Int
getId = ffi ["p"] "p.getId()"

setId :: Int -> PanelPB -> Effect Unit
setId = fpi ["i", "p", ""] "p.setId(i)"

getUUID :: PanelPB -> PBUUID
getUUID = ffi ["p"] "p.getUuid()"

setUUID :: PBUUID -> PanelPB -> Effect Unit
setUUID = fpi ["u", "p", ""] "p.setUuid(u)"

getLeadId :: PanelPB -> Int
getLeadId = ffi ["p"] "p.getLeadId()"

setLeadId :: Int -> PanelPB -> Effect Unit
setLeadId = fpi ["i", "p", ""] "p.setLeadId(i)"

getRoofplateUUID :: PanelPB -> PBUUID
getRoofplateUUID = ffi ["p"] "p.getRoofplateUuid()"

setRoofplateUUID :: PBUUID -> PanelPB -> Effect Unit
setRoofplateUUID = fpi ["u", "p", ""] "p.setRoofplateUuid(u)"

getRowNumber :: PanelPB -> Int
getRowNumber = ffi ["p"] "p.getRowNumber()"

setRowNumber :: Int -> PanelPB -> Effect Unit
setRowNumber = fpi ["r", "p", ""] "p.setRowNumber(r)"

getSlope :: PanelPB -> Number
getSlope = ffi ["p"] "p.getSlope()"

setSlope :: Number -> PanelPB -> Effect Unit
setSlope = fpi ["s", "p", ""] "p.setSlope(s)"

getOrientation :: PanelPB -> OrientationPB
getOrientation = ffi ["p"] "p.getOrientation()"

setOrientation :: OrientationPB -> PanelPB -> Effect Unit
setOrientation = fpi ["o", "p", ""] "p.setOrientation(o)"

getAlignment :: PanelPB -> AlignmentPB
getAlignment = ffi ["p"] "p.getAlignment()"

setAlignment :: AlignmentPB -> PanelPB -> Effect Unit
setAlignment = fpi ["a", "p", ""] "p.setAlignment(a)"
