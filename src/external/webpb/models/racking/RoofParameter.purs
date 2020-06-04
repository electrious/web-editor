module WebPB.Models.Racking.RoofParameter where

import Prelude

import Effect (Effect)
import Util (ffi, fpi)
import WebPB.Models.Racking.BX.Chassis (ChassisKind)
import WebPB.Models.Racking.FX.Mount (MountSpacingPB)
import WebPB.Models.Racking.Rafter (RafterSpacingPB)


foreign import data RoofParameterPB :: Type
foreign import mkRoofParameterPB :: Effect RoofParameterPB

foreign import data ParamTypeCasePB :: Type
foreign import paramTypeNotSet      :: ParamTypeCasePB
foreign import paramTypeXR          :: ParamTypeCasePB
foreign import paramTypeFX          :: ParamTypeCasePB
foreign import paramTypeXRFlat      :: ParamTypeCasePB
foreign import paramTypeBX          :: ParamTypeCasePB
foreign import paramTypeGAF         :: ParamTypeCasePB

class HasSpacings a

getMountSpacing :: forall a. HasSpacings a => a -> MountSpacingPB
getMountSpacing = ffi ["p"] "p.getMountSpace()"

setMountSpacing :: forall a. HasSpacings a => MountSpacingPB -> a -> Effect Unit
setMountSpacing = fpi ["s", "p", ""] "p.setMountSpace(s)"

getRafterSpacing :: forall a. HasSpacings a => a -> RafterSpacingPB
getRafterSpacing = ffi ["p"] "p.getRafterSpace()"

setRafterSpacing :: forall a. HasSpacings a => RafterSpacingPB -> a -> Effect Unit
setRafterSpacing = fpi ["s", "p", ""] "p.setRafterSpace(s)"

foreign import data XRParameterPB :: Type
foreign import mkXRParameterPB :: Effect XRParameterPB

instance hasSpacingsXRParameterPB :: HasSpacings XRParameterPB

foreign import data FXParameterPB :: Type
foreign import mkFXParameterPB :: Effect FXParameterPB
instance hasSpacingsFXParameterPB :: HasSpacings FXParameterPB

foreign import data XRFlatParameterPB :: Type
foreign import mkXRFlatParameterPB :: Effect XRFlatParameterPB

foreign import data BXParameterPB :: Type
foreign import mkBXParameterPB :: Effect BXParameterPB

getChassisKind :: BXParameterPB -> ChassisKind
getChassisKind = ffi ["b"] "b.getChassisKind"

setChassisKind :: ChassisKind -> BXParameterPB -> Effect Unit
setChassisKind = fpi ["c", "b", ""] "b.setChassisKind(c)"

foreign import data GAFParameterPB :: Type
foreign import mkGAFParameterPB :: Effect GAFParameterPB
