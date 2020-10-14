module Model.Racking.MountSpacing where

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

newtype MountSpacingPB = MountSpacingPB Int
derive newtype instance eqMountSpacingPB :: Eq MountSpacingPB

foreign import spacing_Invalid :: MountSpacingPB
foreign import spacing_24 :: MountSpacingPB
foreign import spacing_32 :: MountSpacingPB
foreign import spacing_48 :: MountSpacingPB
foreign import spacing_64 :: MountSpacingPB
foreign import spacing_72 :: MountSpacingPB

data MountSpacing = MountSpacing24
                  | MountSpacing32
                  | MountSpacing48
                  | MountSpacing64
                  | MountSpacing72

derive instance eqMountSpacing :: Eq MountSpacing
derive instance ordMountSpacing :: Ord MountSpacing
derive instance genericMountSpacing :: Generic MountSpacing _
instance showMountSpacing :: Show MountSpacing where
    show = genericShow
instance boundMountSpacing :: Bounded MountSpacing where
    top = genericTop
    bottom = genericBottom
instance enumMountSpacing :: Enum MountSpacing where
    succ = genericSucc
    pred = genericPred
instance boundEnumMountSpacing :: BoundedEnum MountSpacing where
    cardinality = genericCardinality
    toEnum = genericToEnum
    fromEnum = genericFromEnum
instance encodeMountSpacing :: Encode MountSpacing where
    encode = fromEnum >>> encode
instance decodeMountSpacing :: Decode MountSpacing where
    decode o = decode o >>= \i -> do
                    case toEnum i of
                        Just v -> pure v
                        Nothing -> throwError $ singleton $ ForeignError $ "Can't decode MountSpacing from: " <> show i
instance protoDecodableMountSpacing :: ProtoDecodable MountSpacing MountSpacingPB where
    fromProto v | v == spacing_24 = MountSpacing24
                | v == spacing_32 = MountSpacing32
                | v == spacing_48 = MountSpacing48
                | v == spacing_64 = MountSpacing64
                | v == spacing_72 = MountSpacing72
                | otherwise       = MountSpacing24
instance protoEncodableMountSpacing :: ProtoEncodable MountSpacing MountSpacingPB where
    toProto MountSpacing24 = pure spacing_24
    toProto MountSpacing32 = pure spacing_32
    toProto MountSpacing48 = pure spacing_48
    toProto MountSpacing64 = pure spacing_64
    toProto MountSpacing72 = pure spacing_72