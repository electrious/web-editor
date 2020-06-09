module Model.Racking.FX.Mount where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Meter (Meter, inch, meter)
import Data.Newtype (class Newtype)
import Data.UUID (UUID)
import Editor.Common.ProtoCodable (class ProtoDecodable, fromProto)
import Effect (Effect)
import Util (ffi, fpi)
import Model.Class (class HasPBUUID, class IsPBArrayComp, getArrayNumber, getUUID, getX, getY, getZ)
import Model.UUID (PBUUID)

foreign import data MountPB :: Type
foreign import mkMountPB :: Effect MountPB

instance hasPBUUIdMountPB :: HasPBUUID MountPB
instance isPBArrayCompMountPB :: IsPBArrayComp MountPB

getFlash :: MountPB -> PBUUID
getFlash = ffi ["m"] "m.getFlash()"

setFlash :: PBUUID -> MountPB -> Effect Unit
setFlash = fpi ["f", "m", ""] "m.setFlash(f)"

getClampX :: MountPB -> Number
getClampX = ffi ["m"] "m.getClampX()"

setClampX :: Number -> MountPB -> Effect Unit
setClampX = fpi ["x", "m", ""] "m.setClampX(x)"

mountRadius :: Meter
mountRadius = inch 5.0

mountLength :: Meter
mountLength = meter 0.11

mountWidth :: Meter
mountWidth = meter 0.0508

newtype Mount = Mount {
    id          :: UUID,
    x           :: Meter,
    y           :: Meter,
    z           :: Meter,
    arrayNumber :: Int,
    flashId     :: UUID,
    clampX      :: Meter
}

derive instance newtypeMount :: Newtype Mount _
derive instance genericMount :: Generic Mount _
instance showMount :: Show Mount where
    show = genericShow
instance protoDecodableMount :: ProtoDecodable Mount MountPB where
    fromProto m = Mount {
        id          : fromProto $ getUUID m,
        x           : meter $ getX m,
        y           : meter $ getY m,
        z           : meter $ getZ m,
        arrayNumber : getArrayNumber m,
        flashId     : fromProto $ getFlash m,
        clampX      : meter $ getClampX m
    }