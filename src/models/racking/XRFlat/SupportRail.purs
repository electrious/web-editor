module Model.Racking.XRFlat.SupportRail where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Meter (Meter, meter)
import Data.Newtype (class Newtype)
import Data.UUID (UUID)
import Editor.Common.ProtoCodable (class ProtoDecodable, fromProto)
import Effect (Effect)
import Model.Class (class HasLength, class HasPBUUID, class IsPBArrayComp, getArrayNumber, getLength, getUUID, getX, getY, getZ)

foreign import data SupportRailPB :: Type
foreign import mkSupportRailPB :: Effect SupportRailPB

instance hasPBUUIDSupportRailPB :: HasPBUUID SupportRailPB
instance isPBArrayCompSupportRailPB :: IsPBArrayComp SupportRailPB
instance hasLengthSupportRailPB :: HasLength SupportRailPB

newtype SupportRail = SupportRail {
    id          :: UUID,
    x           :: Meter,
    y           :: Meter,
    z           :: Meter,
    arrayNumber :: Int,
    length      :: Meter
}

derive instance newtypeSupportRail :: Newtype SupportRail _
derive instance genericSupportRail :: Generic SupportRail _
instance showSupportRail :: Show SupportRail where
    show = genericShow
instance protoDecodableSupportRail :: ProtoDecodable SupportRail SupportRailPB where
    fromProto s = SupportRail {
        id          : fromProto $ getUUID s,
        x           : meter $ getX s,
        y           : meter $ getY s,
        z           : meter $ getZ s,
        arrayNumber : getArrayNumber s,
        length      : meter $ getLength s
    }
