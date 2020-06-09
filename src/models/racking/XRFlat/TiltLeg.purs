module Model.Racking.XRFlat.TiltLeg where

import Prelude hiding (degree)

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
import Math.Angle (Angle, degree)
import Util (ffi, fpi)
import Model.Class (class HasLength, class HasPBUUID, class IsPBArrayComp, getArrayNumber, getLength, getUUID, getX, getY, getZ)

newtype TiltLegKind = TiltLegKind Int
derive newtype instance eqTiltLegKind :: Eq TiltLegKind
foreign import tiltKindInvalid :: TiltLegKind
foreign import tiltKindNorth :: TiltLegKind
foreign import tiltKindSouth :: TiltLegKind

foreign import data TiltLegPB :: Type
foreign import mkTiltLegPB :: Effect TiltLegPB

instance hasPBUUIDTiltLegPB :: HasPBUUID TiltLegPB
instance isPBArrayCompTiltLegPB :: IsPBArrayComp TiltLegPB
instance hasLengthTiltLegPB :: HasLength TiltLegPB

getTilt :: TiltLegPB -> Number
getTilt = ffi ["t"] "t.getTilt()"

setTilt :: Number -> TiltLegPB -> Effect Unit
setTilt = fpi ["l", "t", ""] "t.setTilt(l)"

getKind :: TiltLegPB -> TiltLegKind
getKind = ffi ["t"] "t.getKind()"

setKind :: TiltLegKind -> TiltLegPB -> Effect Unit
setKind = fpi ["k", "t", ""] "t.setKind(k)"

data TiltLegType = South | North

derive instance eqTiltLegType :: Eq TiltLegType
derive instance ordTiltLegType :: Ord TiltLegType
derive instance genericTiltLegType :: Generic TiltLegType _
instance showTiltLegType :: Show TiltLegType where
    show = genericShow
instance boundTiltLegType :: Bounded TiltLegType where
    top = genericTop
    bottom = genericBottom
instance enumTiltLegType :: Enum TiltLegType where
    succ = genericSucc
    pred = genericPred
instance boundEnumTiltLegType :: BoundedEnum TiltLegType where
    cardinality = genericCardinality
    toEnum = genericToEnum
    fromEnum = genericFromEnum
instance protoDecodableTiltLegType :: ProtoDecodable TiltLegType TiltLegKind where
    fromProto v | v == tiltKindNorth = North
                | v == tiltKindSouth = South
                | otherwise          = South

newtype TiltLeg = TiltLeg {
    id          :: UUID,
    x           :: Meter,
    y           :: Meter,
    z           :: Meter,
    length      :: Meter,
    tilt        :: Angle,
    arrayNumber :: Int,
    type        :: TiltLegType
}

derive instance newtypeTiltLeg :: Newtype TiltLeg _
derive instance genericTiltLeg :: Generic TiltLeg _
instance showTiltLeg :: Show TiltLeg where
    show = genericShow
instance protoDecodableTiltLeg :: ProtoDecodable TiltLeg TiltLegPB where
    fromProto t = TiltLeg {
        id          : fromProto $ getUUID t,
        x           : meter $ getX t,
        y           : meter $ getY t,
        z           : meter $ getZ t,
        length      : meter $ getLength t,
        tilt        : degree $ getTilt t,
        arrayNumber : getArrayNumber t,
        type        : fromProto $ getKind t
    }