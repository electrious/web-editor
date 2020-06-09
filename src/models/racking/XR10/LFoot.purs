module Model.Racking.XR10.LFoot where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Meter (Meter, meter)
import Data.Newtype (class Newtype)
import Data.UUID (UUID)
import Editor.Common.ProtoCodable (class ProtoDecodable, fromProto)
import Effect (Effect)
import Util (ffi, fpi)
import Model.Class (class HasPBUUID, class IsPBArrayComp, getArrayNumber, getUUID, getX, getY, getZ)
import Model.UUID (PBUUID)

foreign import data LFootPB :: Type
foreign import mkLFootPB :: Effect LFootPB

instance hasPBUUIDLFootPB :: HasPBUUID LFootPB
instance isPBArrayCompLFootPB :: IsPBArrayComp LFootPB

getFlashId :: LFootPB -> PBUUID
getFlashId = ffi ["r"] "r.getFlash()"

setFlashId :: PBUUID -> LFootPB -> Effect Unit
setFlashId = fpi ["u", "r", ""] "r.setFlash(u)"

newtype LFoot = LFoot {
    id          :: UUID,
    x           :: Meter,
    y           :: Meter,
    z           :: Meter,
    arrayNumber :: Int,
    flashId     :: UUID
}

derive instance newtypeLFoot :: Newtype LFoot _
derive instance genericLFoot :: Generic LFoot _
instance showLFoot :: Show LFoot where
    show = genericShow
instance protoDecodableLFoot :: ProtoDecodable LFoot LFootPB where
    fromProto l = LFoot {
        id          : fromProto $ getUUID l,
        x           : meter $ getX l,
        y           : meter $ getY l,
        z           : meter $ getZ l,
        arrayNumber : getArrayNumber l,
        flashId     : fromProto $ getFlashId l
    }