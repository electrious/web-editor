module Model.Racking.RackingSystem where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.Map (Map, fromFoldable, toUnfoldable)
import Data.Maybe (fromMaybe)
import Data.Newtype (class Newtype)
import Data.Tuple (Tuple(..))
import Data.UUID (UUID, emptyUUID, parseUUID)
import Editor.Common.ProtoCodable (class ProtoDecodable, fromProto)
import Effect (Effect)
import Model.Racking.RoofRackingData (RoofRackingData, RoofRackingResultPB)
import Model.MapPB (MapPB, fromMapPB)

foreign import data RackingSystemPB :: Type
foreign import mkRackingSystemPB :: Effect RackingSystemPB

foreign import getRoofRackings :: RackingSystemPB -> MapPB String RoofRackingResultPB
foreign import setRoofRackings :: MapPB String RoofRackingResultPB -> RackingSystemPB -> Effect Unit

newtype RackingSystem = RackingSystem {
    roofRackings :: Map UUID RoofRackingData
}

derive instance newtypeRackingSystem :: Newtype RackingSystem _
derive instance genericRackingSystem :: Generic RackingSystem _
instance showRackingSystem :: Show RackingSystem where
    show = genericShow
instance protoDecodableRackingSystem :: ProtoDecodable RackingSystem RackingSystemPB where
    fromProto rs = RackingSystem {
            roofRackings: mapKeyVal f fromProto $ fromMapPB $ getRoofRackings rs
        }
        where f u = fromMaybe emptyUUID $ parseUUID u


mapKeyVal :: forall k v k1 v1. Ord k => Ord k1 => (k -> k1) -> (v -> v1) -> Map k v -> Map k1 v1
mapKeyVal kf vf m = fromFoldable $ f <$> arr
    where f (Tuple k v) = Tuple (kf k) (vf v)
          arr :: Array (Tuple k v)
          arr = toUnfoldable m
