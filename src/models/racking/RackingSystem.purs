module Model.Racking.RackingSystem where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.Map (Map, fromFoldable, toUnfoldable)
import Data.Maybe (fromMaybe)
import Data.Newtype (class Newtype)
import Data.Tuple (Tuple(..))
import Data.UUIDWrapper (UUID, emptyUUID, parseUUID)
import Effect (Effect)
import Model.Racking.RoofRackingData (RoofRackingData)

newtype RackingSystem = RackingSystem {
    roofRackings :: Map UUID RoofRackingData
}

derive instance newtypeRackingSystem :: Newtype RackingSystem _
derive instance genericRackingSystem :: Generic RackingSystem _
instance showRackingSystem :: Show RackingSystem where
    show = genericShow

mapKeyVal :: forall k v k1 v1. Ord k => Ord k1 => (k -> k1) -> (v -> v1) -> Map k v -> Map k1 v1
mapKeyVal kf vf m = fromFoldable $ f <$> arr
    where f (Tuple k v) = Tuple (kf k) (vf v)
          arr :: Array (Tuple k v)
          arr = toUnfoldable m
