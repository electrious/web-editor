module API.V1.Racking.Request where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.Map (Map)
import Data.Newtype (class Newtype)
import Model.Racking.RoofParameter (RoofParameter)
import Model.Roof.Panel (Panel)

newtype RackRequest = RackRequest {
    parameters :: Map String RoofParameter,
    panels     :: Array Panel
}

derive instance newtypeRackRequest :: Newtype RackRequest _
derive instance genericRackRequest :: Generic RackRequest _
instance showRackRequest :: Show RackRequest where
    show = genericShow