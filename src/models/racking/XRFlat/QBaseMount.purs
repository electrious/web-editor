module Model.Racking.XRFlat.QBaseMount where

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

foreign import data QBaseMountPB :: Type
foreign import mkQBaseMountPB :: Effect QBaseMountPB

instance hasPBUUIDQBaseMountPB :: HasPBUUID QBaseMountPB
instance isPBArrayCompQBaseMountPB :: IsPBArrayComp QBaseMountPB

getHeight :: QBaseMountPB -> Number
getHeight = ffi ["q"] "q.getHeight()"

setHeight :: Number -> QBaseMountPB -> Effect Unit
setHeight = fpi ["h", "q", ""] "q.setHeight(h)"

newtype QBaseMount = QBaseMount {
    id          :: UUID,
    x           :: Meter,
    y           :: Meter,
    z           :: Meter,
    arrayNumber :: Int,
    height      :: Meter
}

derive instance newtypeQBaseMount :: Newtype QBaseMount _
derive instance genericQBaseMount :: Generic QBaseMount _
instance showQBaseMount :: Show QBaseMount where
    show = genericShow
instance protoDecodableQBaseMount :: ProtoDecodable QBaseMount QBaseMountPB where
    fromProto q = QBaseMount {
        id          : fromProto $ getUUID q,
        x           : meter $ getX q,
        y           : meter $ getY q,
        z           : meter $ getZ q,
        arrayNumber : getArrayNumber q,
        height      : meter $ getHeight q
    }