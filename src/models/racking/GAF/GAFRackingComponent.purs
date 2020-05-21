module Model.Racking.GAF.GAFRackingComponent where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Newtype (class Newtype)
import Model.Racking.GAF.Hood (Hood)

newtype GAFRackingComponent = GAFRackingComponent {
    arrayNumber :: Int,
    hoods       :: Array Hood
}

derive instance newtypeGAFRackingComponent :: Newtype GAFRackingComponent _
derive instance genericGAFRackingComponent :: Generic GAFRackingComponent _
instance showGAFRackingComponent :: Show GAFRackingComponent where
    show = genericShow

newtype GAFRackingNumbers = GAFRackingNumbers {
    hoods :: Int
}

derive instance newtypeGAFRackingNumbers :: Newtype GAFRackingNumbers _
derive instance genericGAFRackingNumbers :: Generic GAFRackingNumbers _
instance showGAFRackingNumbers :: Show GAFRackingNumbers where
    show = genericShow
