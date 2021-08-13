module Model.Racking.BX.Chassis where

import Prelude

import Data.Argonaut.Core (jsonEmptyObject)
import Data.Argonaut.Decode (class DecodeJson, JsonDecodeError(..), decodeJson, (.:))
import Data.Argonaut.Encode (class EncodeJson, encodeJson, (:=), (~>))
import Data.Bounded.Generic (genericBottom, genericTop)
import Data.Default (def)
import Data.Either (note)
import Data.Enum (class BoundedEnum, class Enum, fromEnum, toEnum)
import Data.Enum.Generic (genericCardinality, genericFromEnum, genericPred, genericSucc, genericToEnum)
import Data.Generic.Rep (class Generic)
import Data.Lens (view, (.~), (^.))
import Data.Meter (Meter, meter)
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
import Data.UUIDWrapper (UUID)
import Editor.Common.Lenses (_arrayNumber, _height, _id, _type, _width, _x, _y, _z)
import Model.ArrayComponent (class ArrayComponent)
import Model.RoofComponent (class RoofComponent)


data ChassisType = ChassisTilt5
                 | ChassisTilt10

derive instance Eq ChassisType
derive instance Ord ChassisType
derive instance Generic ChassisType _
instance Show ChassisType where
    show = genericShow
instance Bounded ChassisType where
    top = genericTop
    bottom = genericBottom
instance Enum ChassisType where
    succ = genericSucc
    pred = genericPred
instance BoundedEnum ChassisType where
    cardinality = genericCardinality
    toEnum = genericToEnum
    fromEnum = genericFromEnum
instance EncodeJson ChassisType where
    encodeJson = fromEnum >>> encodeJson
instance DecodeJson ChassisType where
    decodeJson = decodeJson >=> toEnum >>> note (TypeMismatch "not valid ChassisType value")

newtype Chassis = Chassis {
    id          :: UUID,
    x           :: Meter,
    y           :: Meter,
    z           :: Meter,
    arrayNumber :: Int,
    type        :: ChassisType
}

derive instance Newtype Chassis _
derive instance Generic Chassis _
instance showChassis :: Show Chassis where
    show = genericShow
instance RoofComponent Chassis where
    compId = view _id
    compX  = view _x
    compY  = view _y
    compZ  = view _z
    size _ = def # _width  .~ meter 0.3
                 # _height .~ meter 0.3
instance ArrayComponent Chassis where
    arrayNumber = view _arrayNumber
instance EncodeJson Chassis where
    encodeJson c = "id" := c ^. _id
                ~> "x"  := c ^. _x
                ~> "y"  := c ^. _y
                ~> "z"  := c ^. _z
                ~> "an" := c ^. _arrayNumber
                ~> "t"  := c ^. _type
                ~> jsonEmptyObject
instance DecodeJson Chassis where
    decodeJson = decodeJson >=> f
        where f o = mkChassis <$> o .: "id"
                              <*> o .: "an"
                              <*> o .: "t"
                              <*> o .: "x"
                              <*> o .: "y"
                              <*> o .: "z"

mkChassis :: UUID -> Int -> ChassisType -> Meter -> Meter -> Meter -> Chassis
mkChassis id an t x y z = Chassis { id: id, arrayNumber: an, type: t, x: x, y: y, z: z }