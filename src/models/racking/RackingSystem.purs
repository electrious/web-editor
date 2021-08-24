module Model.Racking.RackingSystem where

import Prelude

import Data.Argonaut.Core (jsonEmptyObject)
import Data.Argonaut.Decode (class DecodeJson, decodeJson, (.:))
import Data.Argonaut.Encode (class EncodeJson, (:=), (~>))
import Data.Generic.Rep (class Generic)
import Data.Map (Map, fromFoldable, toUnfoldable)
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
import Data.Tuple (Tuple(..))
import Data.UUIDMap (fromObject, toObject)
import Data.UUIDWrapper (UUID)
import Model.Racking.RoofRackingData (RoofRackingData)

newtype RackingSystem = RackingSystem {
    roofRackings :: Map UUID RoofRackingData
}

derive instance Newtype RackingSystem _
derive instance Generic RackingSystem _
instance Show RackingSystem where
    show = genericShow
instance EncodeJson RackingSystem where
    encodeJson (RackingSystem r) = "racks" := toObject r.roofRackings ~> jsonEmptyObject
instance DecodeJson RackingSystem where
    decodeJson = decodeJson >=> f
        where f o = mkRackingSystem <<< fromObject <$> o .: "racks"

mkRackingSystem :: Map UUID RoofRackingData -> RackingSystem
mkRackingSystem rs = RackingSystem { roofRackings : rs }

mapKeyVal :: forall k v k1 v1. Ord k => Ord k1 => (k -> k1) -> (v -> v1) -> Map k v -> Map k1 v1
mapKeyVal kf vf m = fromFoldable $ f <$> arr
    where f (Tuple k v) = Tuple (kf k) (vf v)
          arr :: Array (Tuple k v)
          arr = toUnfoldable m
