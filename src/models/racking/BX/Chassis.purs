module Model.Racking.BX.Chassis where

import Prelude

import Control.Monad.Error.Class (throwError)
import Data.Enum (class BoundedEnum, class Enum, fromEnum, toEnum)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Bounded (genericBottom, genericTop)
import Data.Generic.Rep.Enum (genericCardinality, genericFromEnum, genericPred, genericSucc, genericToEnum)
import Data.Generic.Rep.Show (genericShow)
import Data.List.NonEmpty (singleton)
import Data.Maybe (Maybe(..))
import Foreign.Generic (class Decode, class Encode, ForeignError(..), decode, encode)

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