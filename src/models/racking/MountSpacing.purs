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
import Foreign.Generic (class Decode, class Encode, ForeignError(..), decode, encode)

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
