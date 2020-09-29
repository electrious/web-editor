module Model.Racking.RafterSpacing where

import Prelude

import Control.Monad.Error.Class (throwError)
import Data.Enum (class BoundedEnum, class Enum, fromEnum, toEnum)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Bounded (genericBottom, genericTop)
import Data.Generic.Rep.Enum (genericCardinality, genericFromEnum, genericPred, genericSucc, genericToEnum)
import Data.Generic.Rep.Show (genericShow)
import Data.List.NonEmpty (singleton)
import Data.Maybe (Maybe(..))
import Editor.Common.ProtoCodable (class ProtoDecodable, class ProtoEncodable)
import Foreign.Generic (class Decode, class Encode, ForeignError(..), decode, encode)

newtype RafterSpacingPB = RafterSpacingPB Int
derive newtype instance eqRafterSpacingPB :: Eq RafterSpacingPB
foreign import rafterSpacingInvalid :: RafterSpacingPB
foreign import rafterSpacing16 :: RafterSpacingPB
foreign import rafterSpacing24 :: RafterSpacingPB

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
instance protoDecodableRafterSpacing :: ProtoDecodable RafterSpacing RafterSpacingPB where
    fromProto v | v == rafterSpacing16 = RafterSpacing16
                | v == rafterSpacing24 = RafterSpacing24
                | otherwise            = RafterSpacing16
instance protoEncodableRafterSpacing :: ProtoEncodable RafterSpacing RafterSpacingPB where
    toProto RafterSpacing16 = pure rafterSpacing16
    toProto RafterSpacing24 = pure rafterSpacing24
