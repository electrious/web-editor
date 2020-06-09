module Model.Racking.XR10.Clamp where

import Prelude

import Data.Enum (class BoundedEnum, class Enum)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Bounded (genericBottom, genericTop)
import Data.Generic.Rep.Enum (genericCardinality, genericFromEnum, genericPred, genericSucc, genericToEnum)
import Data.Generic.Rep.Show (genericShow)
import Data.Meter (Meter, meter)
import Data.Newtype (class Newtype)
import Data.UUID (UUID)
import Editor.Common.ProtoCodable (class ProtoDecodable, fromProto)
import Effect (Effect)
import Model.Racking.Common (RackPos)
import Util (ffi, fpi)
import Model.Class (class HasPBUUID, class HasPos, class IsPBArrayComp, getArrayNumber, getPos, getUUID, getX, getY, getZ)

newtype ClampKindPB = ClampKindPB Int
derive newtype instance eqClampKindPB :: Eq ClampKindPB

foreign import clampKindInvalid :: ClampKindPB
foreign import clampKindMiddle :: ClampKindPB
foreign import clampKindEnd :: ClampKindPB

foreign import data ClampPB :: Type
foreign import mkClampPB :: Effect ClampPB

instance hasPBUUIDClampPB :: HasPBUUID ClampPB
instance isPBArrayCompClampPB :: IsPBArrayComp ClampPB
instance hasPosClampPB :: HasPos ClampPB

getKind :: ClampPB -> ClampKindPB
getKind = ffi ["r"] "r.getKind()"

setKind :: ClampKindPB -> ClampPB -> Effect Unit
setKind = fpi ["k", "r", ""] "r.setKind(k)"


data ClampType = Middle | End

derive instance eqClampType :: Eq ClampType
derive instance ordClampType :: Ord ClampType
derive instance genericClampType :: Generic ClampType _
instance showClampType :: Show ClampType where
    show = genericShow
instance boundClampType :: Bounded ClampType where
    top = genericTop
    bottom = genericBottom
instance enumClampType :: Enum ClampType where
    succ = genericSucc
    pred = genericPred
instance boundEnumClampType :: BoundedEnum ClampType where
    cardinality = genericCardinality
    toEnum = genericToEnum
    fromEnum = genericFromEnum
instance protoDecodableClampType :: ProtoDecodable ClampType ClampKindPB where
    fromProto v | v == clampKindMiddle  = Middle
                | v == clampKindEnd     = End
                | otherwise             = Middle

newtype Clamp = Clamp {
    id          :: UUID,
    x           :: Meter,
    y           :: Meter,
    z           :: Meter,
    arrayNumber :: Int,
    clampType   :: ClampType,
    clampPos    :: RackPos
}

derive instance newtypeClamp :: Newtype Clamp _
derive instance genericClamp :: Generic Clamp _
instance showClamp :: Show Clamp where
    show = genericShow
instance protoDecodableClamp :: ProtoDecodable Clamp ClampPB where
    fromProto c = Clamp {
        id          : fromProto $ getUUID c,
        x           : meter $ getX c,
        y           : meter $ getY c,
        z           : meter $ getZ c,
        arrayNumber : getArrayNumber c,
        clampType   : fromProto $ getKind c,
        clampPos    : fromProto $ getPos c
    }