module Model.Racking.MountSpacing where

import Prelude

import Data.Argonaut.Decode (class DecodeJson, JsonDecodeError(..), decodeJson)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Bounded.Generic (genericBottom, genericTop)
import Data.Either (note)
import Data.Enum (class BoundedEnum, class Enum, fromEnum, toEnum)
import Data.Enum.Generic (genericCardinality, genericFromEnum, genericPred, genericSucc, genericToEnum)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)


data MountSpacing = MountSpacing24
                  | MountSpacing32
                  | MountSpacing48
                  | MountSpacing64
                  | MountSpacing72

derive instance Eq MountSpacing
derive instance Ord MountSpacing
derive instance Generic MountSpacing _
instance Show MountSpacing where
    show = genericShow
instance Bounded MountSpacing where
    top = genericTop
    bottom = genericBottom
instance Enum MountSpacing where
    succ = genericSucc
    pred = genericPred
instance BoundedEnum MountSpacing where
    cardinality = genericCardinality
    toEnum = genericToEnum
    fromEnum = genericFromEnum
instance EncodeJson MountSpacing where
    encodeJson = fromEnum >>> encodeJson
instance DecodeJson MountSpacing where
    decodeJson = decodeJson >=> toEnum >>> note (TypeMismatch "Incorrect value for MountSpacing")
