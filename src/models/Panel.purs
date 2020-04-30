module Models.Roof.Panel where

import Prelude

import Control.Monad.Error.Class (throwError)
import Data.Enum (class BoundedEnum, class Enum, fromEnum, toEnum)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Bounded (genericBottom, genericTop)
import Data.Generic.Rep.Enum (genericCardinality, genericFromEnum, genericPred, genericSucc, genericToEnum)
import Data.Generic.Rep.Ord (genericCompare)
import Data.Generic.Rep.Show (genericShow)
import Data.Lens (Lens')
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.List.NonEmpty (singleton)
import Data.Maybe (Maybe(..))
import Data.Meter (Meter)
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy(..))
import Foreign.Generic (class Decode, class Encode, ForeignError(..), decode, defaultOptions, encode, genericDecode, genericEncode)
import Math.Angle (Angle)

data Orientation = Landscape
                 | Portrait

derive instance genericOrient :: Generic Orientation _
derive instance eqOrient :: Eq Orientation
instance showOrient :: Show Orientation where
    show = genericShow
instance ordOrient :: Ord Orientation where
    compare = genericCompare
instance boundOrient :: Bounded Orientation where
    top = genericTop
    bottom = genericBottom
instance enumOrient :: Enum Orientation where
    succ = genericSucc
    pred = genericPred
instance boundEnumOrient :: BoundedEnum Orientation where
    cardinality = genericCardinality
    toEnum = genericToEnum
    fromEnum = genericFromEnum
instance encodeOrient :: Encode Orientation where
    encode = fromEnum >>> encode
instance decodeOrient :: Decode Orientation where
    decode o = decode o >>= \i -> do 
                    case toEnum i of
                        Just v -> pure v
                        Nothing -> throwError $ singleton $ ForeignError ("can't decode Orientation from " <> show i)

data Alignment = Grid
               | Brick

derive instance genericAlign :: Generic Alignment _
derive instance eqAlign :: Eq Alignment
instance showAlign :: Show Alignment where
    show = genericShow
instance ordAlign :: Ord Alignment where
    compare = genericCompare
instance enumAlign :: Enum Alignment where
    succ = genericSucc
    pred = genericPred
instance bounedAlign :: Bounded Alignment where
    top = genericTop
    bottom = genericBottom
instance boundedEnumAlign :: BoundedEnum Alignment where
    cardinality = genericCardinality
    toEnum = genericToEnum
    fromEnum = genericFromEnum
instance encodeAlign :: Encode Alignment where
    encode = fromEnum >>> encode
instance decodeAlign :: Decode Alignment where
    decode a = decode a >>= \i -> do
                   case toEnum i of
                      Just v -> pure v
                      Nothing -> throwError $ singleton $ ForeignError ("can't decode Alignment from " <> show i)

newtype Panel = Panel {
    id             :: Int,
    uuid           :: String,
    lead_id        :: Int,
    roofplate_id   :: Int,
    roofplate_uuid :: String,
    row_number     :: Int,
    array_number   :: Int,
    x              :: Meter,
    y              :: Meter,
    slope          :: Angle,
    orientation    :: Orientation,
    alignment      :: Alignment
}

derive instance newtypePanel :: Newtype Panel _
derive instance genericPanel :: Generic Panel _
instance showPanel :: Show Panel where
    show = genericShow
instance encodePanel :: Encode Panel where
    encode = genericEncode (defaultOptions { unwrapSingleConstructors = true })
instance decodePanel :: Decode Panel where
    decode = genericDecode (defaultOptions { unwrapSingleConstructors = true })

_panelId :: Lens' Panel Int
_panelId = _Newtype <<< prop (SProxy :: SProxy "id")

_uuid :: Lens' Panel String
_uuid = _Newtype <<< prop (SProxy :: SProxy "uuid")

_leadId :: Lens' Panel Int
_leadId = _Newtype <<< prop (SProxy :: SProxy "lead_id")

_roofId :: Lens' Panel Int
_roofId = _Newtype <<< prop (SProxy :: SProxy "roofplate_id")

_roofUUID :: Lens' Panel String
_roofUUID = _Newtype <<< prop (SProxy :: SProxy "roofplate_uuid")

_rowNumber :: Lens' Panel Int
_rowNumber = _Newtype <<< prop (SProxy :: SProxy "row_number")

_arrNumber :: Lens' Panel Int
_arrNumber = _Newtype <<< prop (SProxy :: SProxy "array_number")

_panelX :: Lens' Panel Meter
_panelX = _Newtype <<< prop (SProxy :: SProxy "x")

_panelY :: Lens' Panel Meter
_panelY = _Newtype <<< prop (SProxy :: SProxy "y")

_slope :: Lens' Panel Angle
_slope = _Newtype <<< prop (SProxy :: SProxy "slope")

_orientation :: Lens' Panel Orientation
_orientation = _Newtype <<< prop (SProxy :: SProxy "orientation")

_alignment :: Lens' Panel Alignment
_alignment = _Newtype <<< prop (SProxy :: SProxy "alignment")
