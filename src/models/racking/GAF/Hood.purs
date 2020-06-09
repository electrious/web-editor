module Model.Racking.GAF.Hood where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Meter (Meter, meter)
import Data.Newtype (class Newtype)
import Data.UUID (UUID)
import Editor.Common.ProtoCodable (class ProtoDecodable, fromProto)
import Effect (Effect)
import Util (ffi, fpi)
import Model.Class (class HasLength, class HasPBUUID, class IsPBArrayComp, getArrayNumber, getLength, getUUID, getX, getY, getZ)

newtype HoodKind = HoodKind Int
derive newtype instance eqHoodKind :: Eq HoodKind
foreign import hoodKindInvalid :: HoodKind
foreign import hoodKindTop :: HoodKind
foreign import hoodKindBottom :: HoodKind

foreign import data HoodPB :: Type
foreign import mkHoodPB :: Effect HoodPB

instance hasPBUUIDHoodPB :: HasPBUUID HoodPB
instance isPBArrayCompHoodPB :: IsPBArrayComp HoodPB
instance hasLengthHoodPB :: HasLength HoodPB

getKind :: HoodPB -> HoodKind
getKind = ffi ["h"] "h.getKind()"

setKind :: HoodKind -> HoodPB -> Effect Unit
setKind = fpi ["k", "h", ""] "h.setKind(k)"

data HoodType = HoodTop
              | HoodBottom
derive instance eqHoodType :: Eq HoodType
derive instance ordHoodType :: Ord HoodType
derive instance genericHoodType :: Generic HoodType _
instance showHoodType :: Show HoodType where
    show = genericShow
instance protoDecodableHoodType :: ProtoDecodable HoodType HoodKind where
    fromProto v | v == hoodKindTop    = HoodTop
                | v == hoodKindBottom = HoodBottom
                | otherwise           = HoodTop

newtype Hood = Hood {
    id          :: UUID,
    x           :: Meter,
    y           :: Meter,
    z           :: Meter,
    arrayNumber :: Int,
    length      :: Meter,
    type        :: HoodType
}

derive instance newtypeHood :: Newtype Hood _
derive instance genericHood :: Generic Hood _
instance showHood :: Show Hood where
    show = genericShow
instance protoDecodableHood :: ProtoDecodable Hood HoodPB where
    fromProto h = Hood {
        id          : fromProto $ getUUID h,
        x           : meter $ getX h,
        y           : meter $ getY h,
        z           : meter $ getZ h,
        arrayNumber : getArrayNumber h,
        length      : meter $ getLength h,
        type        : fromProto $ getKind h
    }