module Model.Racking.BX.BXRackingComponent where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.Newtype (class Newtype)
import Model.Racking.BX.Block (Block)
import Model.Racking.BX.Chassis (Chassis)

newtype BXRackingComponent = BXRackingComponent {
    arrayNumber :: Int,
    chassis     :: Array Chassis,
    blocks      :: Array Block
}

derive instance newtypeBXRackingComponent :: Newtype BXRackingComponent _
derive instance genericBXRackingComponent :: Generic BXRackingComponent _
instance showBXRackingComponent :: Show BXRackingComponent where
    show = genericShow


newtype BXRackingNumbers = BXRackingNumbers {
    chassis :: Int,
    blocks  :: Int
}

derive instance newtypeBXRackingNumbers :: Newtype BXRackingNumbers _
derive instance genericBXRackingNumbers :: Generic BXRackingNumbers _
instance showBXRackingNumbers :: Show BXRackingNumbers where
    show = genericShow
