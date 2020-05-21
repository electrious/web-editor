module Model.Racking.RackingSystem where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Map (Map)
import Data.Newtype (class Newtype)
import Data.UUID (UUID)
import Model.Racking.RoofRackingData (RoofRackingData)

newtype RackingSystem = RackingSystem {
    roofRackings :: Map UUID RoofRackingData
}

derive instance newtypeRackingSystem :: Newtype RackingSystem _
derive instance genericRackingSystem :: Generic RackingSystem _
instance showRackingSystem :: Show RackingSystem where
    show = genericShow
