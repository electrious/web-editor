module Model.Racking.RafterSpacing where

import Prelude

import Data.Argonaut.Decode (class DecodeJson, JsonDecodeError(..), decodeJson)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Bounded.Generic (genericBottom, genericTop)
import Data.Either (note)
import Data.Enum (class BoundedEnum, class Enum, fromEnum, toEnum)
import Data.Enum.Generic (genericCardinality, genericFromEnum, genericPred, genericSucc, genericToEnum)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)

data RafterSpacing = RafterSpacing16
                   | RafterSpacing24

derive instance Eq RafterSpacing
derive instance Ord RafterSpacing
derive instance Generic RafterSpacing _
instance Show RafterSpacing where
    show = genericShow
instance Bounded RafterSpacing where
    top = genericTop
    bottom = genericBottom
instance Enum RafterSpacing where
    succ = genericSucc
    pred = genericPred
instance BoundedEnum RafterSpacing where
    cardinality = genericCardinality
    toEnum = genericToEnum
    fromEnum = genericFromEnum
instance EncodeJson RafterSpacing where
    encodeJson = fromEnum >>> encodeJson
instance DecodeJson RafterSpacing where
    decodeJson = decodeJson >=> toEnum >>> note (TypeMismatch "Invalid value for RafterSpacing")