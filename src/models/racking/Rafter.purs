module Model.Racking.Rafter where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Meter (Meter, meter)
import Data.Newtype (class Newtype)
import Data.UUID (UUID)
import Editor.Common.ProtoCodable (class ProtoDecodable, fromProto)
import Effect (Effect)
import Model.Class (class HasLength, class HasPBUUID, class IsPBArrayComp, getLength, getUUID, getX, getY, getZ)

foreign import data RafterPB :: Type

foreign import mkRafterPB :: Effect RafterPB

instance hasPBUUIDRafterPB :: HasPBUUID RafterPB
instance isPBArrayCompRafterPB :: IsPBArrayComp RafterPB
instance hasLengthRafterPB :: HasLength RafterPB

newtype Rafter = Rafter {
    id     :: UUID,
    x      :: Meter,
    y      :: Meter,
    z      :: Meter,
    length :: Meter
}

derive instance newtypeRafter :: Newtype Rafter _
derive instance genericRafter :: Generic Rafter _
instance showRafter :: Show Rafter where
    show = genericShow
instance protoDecodableRafter :: ProtoDecodable Rafter RafterPB where
    fromProto r = Rafter {
        id     : fromProto $ getUUID r,
        x      : meter $ getX r,
        y      : meter $ getY r,
        z      : meter $ getZ r,
        length : meter $ getLength r
    }
