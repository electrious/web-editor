module Model.Racking.Flash where

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

foreign import data FlashPB :: Type
foreign import mkFlashPB :: Effect FlashPB

instance hasPBUUIDFlashPB :: HasPBUUID FlashPB
instance isPBArrayCompFlashPB :: IsPBArrayComp FlashPB

getRafterId :: FlashPB -> PBUUID
getRafterId = ffi ["r"] "r.getRafterId()"

setRafterId :: PBUUID -> FlashPB -> Effect Unit
setRafterId = fpi ["u", "r", ""] "r.setRafterId(u)"

getClampTarget :: FlashPB -> Number
getClampTarget = ffi ["r"] "r.getClampTarget()"

setClampTarget :: Number -> FlashPB -> Effect Unit
setClampTarget = fpi ["t", "r", ""] "r.setClampTarget(t)"

newtype Flash = Flash {
    id          :: UUID,
    rafterId    :: UUID,
    x           :: Meter,
    y           :: Meter,
    z           :: Meter,
    arrayNumber :: Int
}

derive instance newtypeFlash :: Newtype Flash _
derive instance genericFlash :: Generic Flash _
instance showFlash :: Show Flash where
    show = genericShow
instance protoDecodableFlash :: ProtoDecodable Flash FlashPB where
    fromProto f = Flash {
        id          : fromProto $ getUUID f,
        rafterId    : fromProto $ getRafterId f,
        x           : meter $ getX f,
        y           : meter $ getY f,
        z           : meter $ getZ f,
        arrayNumber : getArrayNumber f
    }