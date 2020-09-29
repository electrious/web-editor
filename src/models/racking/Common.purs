module Model.Racking.Common where

import Prelude

import Data.Enum (class BoundedEnum, class Enum)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Bounded (genericBottom, genericTop)
import Data.Generic.Rep.Enum (genericCardinality, genericFromEnum, genericPred, genericSucc, genericToEnum)
import Data.Generic.Rep.Show (genericShow)
import Editor.Common.ProtoCodable (class ProtoDecodable)

newtype LRPB = LRPB Int
derive newtype instance eqLRPB :: Eq LRPB
foreign import lrInvalid :: LRPB
foreign import lrLeft :: LRPB
foreign import lrRight :: LRPB


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
instance protoDecodableRackPos :: ProtoDecodable RackPos LRPB where
    fromProto v | v == lrLeft  = Left
                | v == lrRight = Right
                | otherwise    = Left
