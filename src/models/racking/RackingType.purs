module Model.Racking.RackingType where

import Prelude

import Control.Monad.Error.Class (throwError)
import Data.Enum (class BoundedEnum, class Enum, fromEnum, toEnum)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Bounded (genericBottom, genericTop)
import Data.Generic.Rep.Enum (genericCardinality, genericFromEnum, genericPred, genericSucc, genericToEnum)
import Data.Generic.Rep.Ord (genericCompare)
import Data.Generic.Rep.Show (genericShow)
import Data.List.NonEmpty (singleton)
import Data.Maybe (Maybe(..))
import Foreign.Generic (class Decode, class Encode, ForeignError(..), decode, encode)

data RackingType = FX
                 | XR10
                 | XRFlat
                 | BX
                 | GAF

derive instance eqRackingType :: Eq RackingType
derive instance genericRackingType :: Generic RackingType _
instance showRackingType :: Show RackingType where
    show = genericShow
instance ordRackingType :: Ord RackingType where
    compare = genericCompare
instance boundRackingType :: Bounded RackingType where
    top = genericTop
    bottom = genericBottom
instance enumRackingType :: Enum RackingType where
    succ = genericSucc
    pred = genericPred
instance boundEnumRackingType :: BoundedEnum RackingType where
    cardinality = genericCardinality
    toEnum = genericToEnum
    fromEnum = genericFromEnum
instance encodeRackingType :: Encode RackingType where
    encode = fromEnum >>> encode
instance decodeRackingType :: Decode RackingType where
    decode o = decode o >>= \i -> do
                    case toEnum i of
                        Just v -> pure v
                        Nothing -> throwError $ singleton $ ForeignError ("can't decode RackingType from: " <> show i)

forNormalRoof :: RackingType -> Boolean
forNormalRoof FX   = true
forNormalRoof XR10 = true
forNormalRoof _    = false

forFlatRoof :: RackingType -> Boolean
forFlatRoof = not <<< forNormalRoof