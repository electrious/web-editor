module Model.Racking.GAF.Hood where

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
import Data.Lens (view, (^.), (.~))
import Data.Meter (Meter, meter)
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
import Data.UUIDWrapper (UUID)
import Editor.Common.Lenses (_arrayNumber, _height, _id, _length, _width, _x, _y, _z)
import Model.ArrayComponent (class ArrayComponent)
import Model.RoofComponent (class RoofComponent)

data HoodType = HoodTop
              | HoodBottom
derive instance Eq HoodType
derive instance Ord HoodType
derive instance Generic HoodType _
instance Show HoodType where
    show = genericShow
instance Bounded HoodType where
    top = genericTop
    bottom = genericBottom
instance Enum HoodType where
    succ = genericSucc
    pred = genericPred
instance BoundedEnum HoodType where
    cardinality = genericCardinality
    toEnum = genericToEnum
    fromEnum = genericFromEnum
instance EncodeJson HoodType where
    encodeJson = fromEnum >>> encodeJson
instance DecodeJson HoodType where
    decodeJson = decodeJson >=> toEnum >>> note (TypeMismatch "Invalid value for HoodType")

newtype Hood = Hood {
    id          :: UUID,
    x           :: Meter,
    y           :: Meter,
    z           :: Meter,
    arrayNumber :: Int,
    length      :: Meter,
    type        :: HoodType
}

derive instance Newtype Hood _
derive instance Generic Hood _
instance showHood :: Show Hood where
    show = genericShow
instance RoofComponent Hood where
    compId = view _id
    compX  = view _x
    compY  = view _y
    compZ  = view _z
    size h = def # _width  .~ h ^. _length
                 # _height .~ meter 0.1
instance ArrayComponent Hood where
    arrayNumber = view _arrayNumber
instance EncodeJson Hood where
    encodeJson (Hood h) = "id" := h.id
                       ~> "x"  := h.x
                       ~> "y"  := h.y
                       ~> "z"  := h.z
                       ~> "an" := h.arrayNumber
                       ~> "l"  := h.length
                       ~> "t"  := h.type
                       ~> jsonEmptyObject
instance DecodeJson Hood where
    decodeJson = decodeJson >=> f
        where f o = mkHood <$> o .: "id"
                           <*> o .: "an"
                           <*> o .: "t"
                           <*> o .: "x"
                           <*> o .: "y"
                           <*> o .: "z"
                           <*> o .: "l"

mkHood :: UUID -> Int -> HoodType -> Meter -> Meter -> Meter -> Meter -> Hood
mkHood id arrayNumber t x y z length = Hood { id: id, x: x, y: y, z: z, arrayNumber: arrayNumber, length: length, type: t }