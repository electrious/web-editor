module Model.Racking.FX.EndCap where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Meter (Meter, meter)
import Data.Newtype (class Newtype)
import Data.UUID (UUID)
import Editor.Common.ProtoCodable (class ProtoDecodable, fromProto)
import Effect (Effect)
import Model.Racking.Common (RackPos)
import Util (ffi, fpi)
import Model.Class (class HasPBUUID, class HasPos, class IsPBArrayComp, getArrayNumber, getPos, getUUID, getX, getY, getZ)
import Model.UUID (PBUUID)

foreign import data EndCapPB :: Type
foreign import mkEndCapPB :: Effect EndCapPB

instance hasPBUUIdEndCapPB :: HasPBUUID EndCapPB
instance isPBArrayCompEndCapPB :: IsPBArrayComp EndCapPB
instance hasPosEndCapPB :: HasPos EndCapPB

getSkirt :: EndCapPB -> PBUUID
getSkirt = ffi ["e"] "e.getSkirt()"

setSkirt :: PBUUID -> EndCapPB -> Effect Unit
setSkirt = fpi ["s", "e", ""] "e.setSkirt(s)"

newtype EndCap = EndCap {
    id          :: UUID,
    x           :: Meter,
    y           :: Meter,
    z           :: Meter,
    arrayNumber :: Int,
    skirtId     :: UUID,
    position    :: RackPos
}

derive instance newtypeEndCap :: Newtype EndCap _
derive instance genericEndCap :: Generic EndCap _
instance showEndCap :: Show EndCap where
    show = genericShow
instance protoDecodableEndCap :: ProtoDecodable EndCap EndCapPB where
    fromProto e = EndCap {
        id          : fromProto $ getUUID e,
        x           : meter $ getX e,
        y           : meter $ getY e,
        z           : meter $ getZ e,
        arrayNumber : getArrayNumber e,
        skirtId     : fromProto $ getSkirt e,
        position    : fromProto $ getPos e
    }