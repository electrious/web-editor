module Model.Racking.Common where

import Prelude

import Data.Argonaut.Decode (class DecodeJson, JsonDecodeError(..), decodeJson)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Bounded.Generic (genericBottom, genericTop)
import Data.Either (note)
import Data.Enum (class BoundedEnum, class Enum, fromEnum, toEnum)
import Data.Enum.Generic (genericCardinality, genericFromEnum, genericPred, genericSucc, genericToEnum)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)

data RackPos = Left | Right

derive instance eqRackPos :: Eq RackPos
derive instance ordRackPos :: Ord RackPos
derive instance genericRackPos :: Generic RackPos _
instance showRackPos :: Show RackPos where
    show = genericShow
instance boundRackPos :: Bounded RackPos where
    top = genericTop
    bottom = genericBottom
instance enumRackPos :: Enum RackPos where
    succ = genericSucc
    pred = genericPred
instance boundEnumRackPos :: BoundedEnum RackPos where
    cardinality = genericCardinality
    toEnum = genericToEnum
    fromEnum = genericFromEnum
instance EncodeJson RackPos where
    encodeJson = fromEnum >>> encodeJson
instance DecodeJson RackPos where
    decodeJson = decodeJson >=> toEnum >>> note (TypeMismatch "invalid RackPos value")