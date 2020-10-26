module Model.Racking.GAF.GAFRackingComponent where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Newtype (class Newtype)
import Editor.Common.ProtoCodable (class ProtoDecodable, fromProto)
import Effect (Effect)
import Model.Racking.Class (class HasArrayNumber, getArrayNumber)
import Model.Racking.GAF.Hood (Hood, HoodPB)

foreign import data GAFComponentPB :: Type
foreign import mkGAFComponentPB :: Effect GAFComponentPB

instance hasArrayNumberGAFComponentPG :: HasArrayNumber GAFComponentPB

foreign import getHoods :: GAFComponentPB -> Array HoodPB
foreign import setHoods :: Array HoodPB -> GAFComponentPB -> Effect Unit

newtype GAFRackingComponent = GAFRackingComponent {
    arrayNumber :: Int,
    hoods       :: Array Hood
}

derive instance newtypeGAFRackingComponent :: Newtype GAFRackingComponent _
derive instance genericGAFRackingComponent :: Generic GAFRackingComponent _
instance showGAFRackingComponent :: Show GAFRackingComponent where
    show = genericShow
instance protoDecodableGAFRackingComponent :: ProtoDecodable GAFRackingComponent GAFComponentPB where
    fromProto c = GAFRackingComponent {
        arrayNumber : getArrayNumber c,
        hoods       : fromProto <$> getHoods c
    }

newtype GAFRackingNumbers = GAFRackingNumbers {
    hoods :: Int
}

derive instance newtypeGAFRackingNumbers :: Newtype GAFRackingNumbers _
derive instance genericGAFRackingNumbers :: Generic GAFRackingNumbers _
instance showGAFRackingNumbers :: Show GAFRackingNumbers where
    show = genericShow
