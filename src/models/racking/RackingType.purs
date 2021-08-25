module Model.Racking.RackingType where

import Prelude

import Data.Argonaut.Decode (class DecodeJson, JsonDecodeError(..), decodeJson)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Bounded.Generic (genericBottom, genericTop)
import Data.Either (note)
import Data.Enum (class BoundedEnum, class Enum, fromEnum, toEnum)
import Data.Enum.Generic (genericCardinality, genericFromEnum, genericPred, genericSucc, genericToEnum)
import Data.Generic.Rep (class Generic)
import Data.Ord.Generic (genericCompare)
import Data.Show.Generic (genericShow)

data RackingType = FX
                 | XR10
                 | XRFlat
                 | BX
                 | GAF

derive instance Eq RackingType
derive instance Generic RackingType _
instance Show RackingType where
    show = genericShow
instance Ord RackingType where
    compare = genericCompare
instance Bounded RackingType where
    top = genericTop
    bottom = genericBottom
instance Enum RackingType where
    succ = genericSucc
    pred = genericPred
instance BoundedEnum RackingType where
    cardinality = genericCardinality
    toEnum = genericToEnum
    fromEnum = genericFromEnum
instance EncodeJson RackingType where
    encodeJson = fromEnum >>> encodeJson
instance DecodeJson RackingType where
    decodeJson = decodeJson >=> toEnum >>> note (TypeMismatch "Invalid value for RackingType")

forNormalRoof :: RackingType -> Boolean
forNormalRoof FX   = true
forNormalRoof XR10 = true
forNormalRoof _    = false

forFlatRoof :: RackingType -> Boolean
forFlatRoof = not <<< forNormalRoof
