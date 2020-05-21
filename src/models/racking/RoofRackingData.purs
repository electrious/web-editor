module Model.Racking.RoofRackingData where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Map (Map)
import Data.Newtype (class Newtype)
import Model.Racking.BX.BXRackingComponent (BXRackingComponent, BXRackingNumbers)
import Model.Racking.FX.FXRackingComponent (FXRackingComponent, FXRackingNumbers)
import Model.Racking.GAF.GAFRackingComponent (GAFRackingComponent, GAFRackingNumbers)
import Model.Racking.RackingType (RackingType)
import Model.Racking.Rafter (Rafter)
import Model.Racking.RoofParameter (RoofParameter)
import Model.Racking.XR10.XRRackingComponent (XRRackingComponent, XRRackingNumbers)
import Model.Racking.XRFlat.XRFlatRackingComponent (XRFlatRackingComponent, XRFlatRackingNumbers)

data RackingComp = FX FXRackingComponent
                 | XR XRRackingComponent
                 | XRFlat XRFlatRackingComponent
                 | BX BXRackingComponent
                 | GAF GAFRackingComponent

derive instance genericRackingComp :: Generic RackingComp _
instance showRackingComp :: Show RackingComp where
    show = genericShow

data RackingCompNumbers = FXNum FXRackingNumbers
                        | XRNum XRRackingNumbers
                        | XRFlatNum XRFlatRackingNumbers
                        | BXNum BXRackingNumbers
                        | GAFNum GAFRackingNumbers

derive instance genericRackingCompNumbers :: Generic RackingCompNumbers _
instance showRackingCompNumbers :: Show RackingCompNumbers where
    show = genericShow


newtype RoofRackingData = RoofRackingData {
    rackingType :: RackingType,
    rafters     :: Array Rafter,
    parameters  :: RoofParameter,
    arrayComps  :: Map Int RackingComp
}

derive instance newtypeRoofRackingData :: Newtype RoofRackingData _
derive instance genericRoofRackingData :: Generic RoofRackingData _
instance showRoofRackingData :: Show RoofRackingData where
    show = genericShow
