module Model.Racking.FX.Skirt where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Meter (Meter, meter)
import Data.Newtype (class Newtype)
import Data.UUID (UUID)
import Editor.Common.ProtoCodable (class ProtoDecodable, fromProto)
import Effect (Effect)
import Model.Class (class HasLength, class HasPBUUID, class IsPBArrayComp, getArrayNumber, getLength, getUUID, getX, getY, getZ)


foreign import data SkirtPB :: Type
foreign import mkSkirtPB :: Effect SkirtPB

instance hasPBUUIDSkirtPB :: HasPBUUID SkirtPB
instance isPBArrayCompSkirtPB :: IsPBArrayComp SkirtPB
instance hasLengthSkirtPB :: HasLength SkirtPB

newtype Skirt = Skirt {
    id          :: UUID,
    x           :: Meter,
    y           :: Meter,
    z           :: Meter,
    arrayNumber :: Int,
    length      :: Meter
}

derive instance newtypeSkirt :: Newtype Skirt _
derive instance genericSKirt :: Generic Skirt _
instance showSkirt :: Show Skirt where
    show = genericShow
instance protoDecodableSkirt :: ProtoDecodable Skirt SkirtPB where
    fromProto s = Skirt {
        id          : fromProto $ getUUID s,
        x           : meter $ getX s,
        y           : meter $ getY s,
        z           : meter $ getZ s,
        arrayNumber : getArrayNumber s,
        length      : meter $ getLength s
    }