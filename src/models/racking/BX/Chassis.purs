module Model.Racking.BX.Chassis where

import Prelude

import Control.Monad.Error.Class (throwError)
import Data.Default (def)
import Data.Enum (class BoundedEnum, class Enum, fromEnum, toEnum)
import Data.Generic.Rep (class Generic)
import Data.Bounded.Generic (genericBottom, genericTop)
import Data.Enum.Generic (genericCardinality, genericFromEnum, genericPred, genericSucc, genericToEnum)
import Data.Show.Generic (genericShow)
import Data.Lens (view, (.~))
import Data.List.NonEmpty (singleton)
import Data.Maybe (Maybe(..))
import Data.Meter (Meter, meter)
import Data.Newtype (class Newtype)
import Data.UUID (UUID)
import Editor.Common.Lenses (_arrayNumber, _height, _id, _width, _x, _y, _z)
import Editor.Common.ProtoCodable (class ProtoDecodable, class ProtoEncodable, fromProto)
import Effect (Effect)
import Foreign.Generic (class Decode, class Encode, ForeignError(..), decode, encode)
import Model.ArrayComponent (class ArrayComponent)
import Model.Class (class HasLength, class HasPBUUID, class IsPBArrayComp, getArrayNumber, getUUID, getX, getY, getZ)
import Model.RoofComponent (class RoofComponent)

newtype ChassisKind = ChassisKind Int
derive newtype instance eqChassisKind :: Eq ChassisKind
foreign import chassisKindInvalid :: ChassisKind
foreign import chassisKindTilt5 :: ChassisKind
foreign import chassisKindTilt10 :: ChassisKind

foreign import data ChassisPB :: Type
foreign import mkChassisPB :: Effect ChassisPB

instance hasPBUUIDChassisPB :: HasPBUUID ChassisPB
instance isPBArrayCompChassisPB :: IsPBArrayComp ChassisPB
instance hasLengthChassisPB :: HasLength ChassisPB

foreign import getTilt :: ChassisPB -> Number
foreign import setTilt :: Number -> ChassisPB -> Effect Unit
foreign import getKind :: ChassisPB -> ChassisKind
foreign import setKind :: ChassisKind -> ChassisPB -> Effect Unit

data ChassisType = ChassisTilt5
                 | ChassisTilt10

derive instance eqChassisType :: Eq ChassisType
derive instance ordChassisType :: Ord ChassisType
derive instance genericChassisType :: Generic ChassisType _
instance showChassisType :: Show ChassisType where
    show = genericShow
instance boundChassisType :: Bounded ChassisType where
    top = genericTop
    bottom = genericBottom
instance enumChassisType :: Enum ChassisType where
    succ = genericSucc
    pred = genericPred
instance boundEnumChassisType :: BoundedEnum ChassisType where
    cardinality = genericCardinality
    toEnum = genericToEnum
    fromEnum = genericFromEnum
instance encodeChassisType :: Encode ChassisType where
    encode = fromEnum >>> encode
instance decodeChassisType :: Decode ChassisType where
    decode o = decode o >>= \i -> do
                    case toEnum i of
                        Just v -> pure v
                        Nothing -> throwError $ singleton $ ForeignError $ "Can't decode ChassisType from: " <> show i
instance protoDecodableChassisType :: ProtoDecodable ChassisType ChassisKind where
    fromProto v | v == chassisKindTilt5  = ChassisTilt5
                | v == chassisKindTilt10 = ChassisTilt10
                | otherwise              = ChassisTilt5
instance protoEncodableChassisType :: ProtoEncodable ChassisType ChassisKind where
    toProto ChassisTilt5  = pure chassisKindTilt5
    toProto ChassisTilt10 = pure chassisKindTilt10

newtype Chassis = Chassis {
    id          :: UUID,
    x           :: Meter,
    y           :: Meter,
    z           :: Meter,
    arrayNumber :: Int,
    type        :: ChassisType
}

derive instance newtypeChassis :: Newtype Chassis _
derive instance genericChassis :: Generic Chassis _
instance showChassis :: Show Chassis where
    show = genericShow
instance roofComponentChassis :: RoofComponent Chassis where
    compId = view _id
    compX  = view _x
    compY  = view _y
    compZ  = view _z
    size _ = def # _width  .~ meter 0.3
                 # _height .~ meter 0.3
instance arrayComponentChassis :: ArrayComponent Chassis where
    arrayNumber = view _arrayNumber
instance protoDecodableChassis :: ProtoDecodable Chassis ChassisPB where
    fromProto c = Chassis {
        id          : fromProto $ getUUID c,
        x           : meter $ getX c,
        y           : meter $ getY c,
        z           : meter $ getZ c,
        arrayNumber : getArrayNumber c,
        type        : fromProto $ getKind c
    }