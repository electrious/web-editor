module Model.Racking.RafterSpacing where

import Prelude

import Control.Monad.Error.Class (throwError)
import Data.Enum (class BoundedEnum, class Enum, fromEnum, toEnum)
import Data.Generic.Rep (class Generic)
import Data.Bounded.Generic (genericBottom, genericTop)
import Data.Enum.Generic (genericCardinality, genericFromEnum, genericPred, genericSucc, genericToEnum)
import Data.Show.Generic (genericShow)
import Data.List.NonEmpty (singleton)
import Data.Maybe (Maybe(..))
import Foreign.Generic (class Decode, class Encode, ForeignError(..), decode, encode)

data RafterSpacing = RafterSpacing16
                   | RafterSpacing24

derive instance eqRafterSpacing :: Eq RafterSpacing
derive instance ordRafterSpacing :: Ord RafterSpacing
derive instance genericRafterSpacing :: Generic RafterSpacing _
instance showRafterSpacing :: Show RafterSpacing where
    show = genericShow
instance boundRafterSpacing :: Bounded RafterSpacing where
    top = genericTop
    bottom = genericBottom
instance enumRafterSpacing :: Enum RafterSpacing where
    succ = genericSucc
    pred = genericPred
instance boundEnumRafterSpacing :: BoundedEnum RafterSpacing where
    cardinality = genericCardinality
    toEnum = genericToEnum
    fromEnum = genericFromEnum
instance encodeRafterSpacing :: Encode RafterSpacing where
    encode = fromEnum >>> encode
instance decodeRafterSpacing :: Decode RafterSpacing where
    decode o = decode o >>= \i -> do
                case toEnum i of
                    Just v -> pure v
                    Nothing -> throwError $ singleton $ ForeignError $ "Can't decode RafterSpacing from: " <> show i
