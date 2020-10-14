module Model.Racking.RoofRackingData where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Map (Map)
import Data.Newtype (class Newtype)
import Editor.Common.ProtoCodable (class ProtoDecodable, fromProto)
import Effect (Effect)
import Model.Racking.BX.BXRackingComponent (BXRackingComponent, BXRackingNumbers, BallastComponentPB)
import Model.Racking.FX.FXRackingComponent (FXRackingComponent, FXRackingNumbers, RailFreeComponentPB)
import Model.Racking.GAF.GAFRackingComponent (GAFComponentPB, GAFRackingComponent, GAFRackingNumbers)
import Model.Racking.RackingType (RackingKind, RackingType)
import Model.Racking.Rafter (Rafter, RafterPB)
import Model.Racking.RoofParameter (RoofParameter, RoofParameterPB)
import Model.Racking.XR10.XRRackingComponent (RailComponentPB, XRRackingComponent, XRRackingNumbers)
import Model.Racking.XRFlat.XRFlatRackingComponent (RailFlatComponentPB, XRFlatRackingComponent, XRFlatRackingNumbers)
import Util (ffi, fpi)
import Model.MapPB (MapPB, fromMapPB)

foreign import data ComponentPB :: Type
foreign import mkComponentPB    :: Effect ComponentPB

newtype RdTypeCase = RdTypeCase Int
derive newtype instance eqRdTypeCase :: Eq RdTypeCase
foreign import rdTypeNotSet   :: RdTypeCase
foreign import rdTypeRail     :: RdTypeCase
foreign import rdTypeRailFree :: RdTypeCase
foreign import rdTypeRailFlat :: RdTypeCase
foreign import rdTypeBallast  :: RdTypeCase
foreign import rdTypeGAF      :: RdTypeCase

getRdTypeCase :: ComponentPB -> RdTypeCase
getRdTypeCase = ffi ["c"] "c.getRdTypeCase()"

getRail :: ComponentPB -> RailComponentPB
getRail = ffi ["c"] "c.getRail()"

setRail :: RailComponentPB -> ComponentPB -> Effect Unit
setRail = fpi ["r", "c", ""] "c.setRail(r)"

getRailFree :: ComponentPB -> RailFreeComponentPB
getRailFree = ffi ["c"] "c.getRailFree()"

setRailFree :: RailFreeComponentPB -> ComponentPB -> Effect Unit
setRailFree = fpi ["r", "c", ""] "c.setRailFree(r)"

getRailFlat :: ComponentPB -> RailFlatComponentPB
getRailFlat = ffi ["c"] "c.getRailFlat()"

setRailFlat :: RailFlatComponentPB -> ComponentPB -> Effect Unit
setRailFlat = fpi ["r", "c", ""] "c.setRailFlat(r)"

getBallast :: ComponentPB -> BallastComponentPB
getBallast = ffi ["c"] "c.getBallast()"

setBallast :: BallastComponentPB -> ComponentPB -> Effect Unit
setBallast = fpi ["b", "c", ""] "c.setBallast(b)"

getGAF :: ComponentPB -> GAFComponentPB
getGAF = ffi ["c"] "c.getGaf()"

setGAF :: GAFComponentPB -> ComponentPB -> Effect Unit
setGAF = fpi ["g", "c", ""] "c.setGaf(g)"


data RackingComp = FX FXRackingComponent
                 | XR XRRackingComponent
                 | XRFlat XRFlatRackingComponent
                 | BX BXRackingComponent
                 | GAF GAFRackingComponent

derive instance genericRackingComp :: Generic RackingComp _
instance showRackingComp :: Show RackingComp where
    show = genericShow
instance protoDecodableRackingComp :: ProtoDecodable RackingComp ComponentPB where
    fromProto c = f $ getRdTypeCase c
        where f v | v == rdTypeRail     = XR $ fromProto $ getRail c
                  | v == rdTypeRailFree = FX $ fromProto $ getRailFree c
                  | v == rdTypeRailFlat = XRFlat $ fromProto $ getRailFlat c
                  | v == rdTypeBallast  = BX $ fromProto $ getBallast c
                  | v == rdTypeGAF      = GAF $ fromProto $ getGAF c
                  | otherwise           = XR $ fromProto $ getRail c

data RackingCompNumbers = FXNum FXRackingNumbers
                        | XRNum XRRackingNumbers
                        | XRFlatNum XRFlatRackingNumbers
                        | BXNum BXRackingNumbers
                        | GAFNum GAFRackingNumbers

derive instance genericRackingCompNumbers :: Generic RackingCompNumbers _
instance showRackingCompNumbers :: Show RackingCompNumbers where
    show = genericShow


foreign import data RoofRackingResultPB :: Type
foreign import mkRoofRackingResultPB :: Effect RoofRackingResultPB

getKind :: RoofRackingResultPB -> RackingKind
getKind = ffi ["r"] "r.getKind()"

setKind :: RackingKind -> RoofRackingResultPB -> Effect Unit
setKind = fpi ["k", "r", ""] "r.setKind(k)"

getRafters :: RoofRackingResultPB -> Array RafterPB
getRafters = ffi ["r"] "r.getRaftersList()"

setRafters :: Array RafterPB -> RoofRackingResultPB -> Effect Unit
setRafters = ffi ["rs", "r", ""] "r.setRaftersList(rs)"

getParams :: RoofRackingResultPB -> RoofParameterPB
getParams = ffi ["r"] "r.getParams()"

setParams :: RoofParameterPB -> RoofRackingResultPB -> Effect Unit
setParams = fpi ["p", "r", ""] "r.setParams(p)"

getComponents :: RoofRackingResultPB -> MapPB Int ComponentPB
getComponents = ffi ["r"] "r.getComponents()"

setComponents :: MapPB Int ComponentPB -> RoofRackingResultPB -> Effect Unit
setComponents = fpi ["c", "r", ""] "r.setComponents(c)"


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
instance protoDecodableRoofRackingData :: ProtoDecodable RoofRackingData RoofRackingResultPB where
    fromProto r = RoofRackingData {
        rackingType : fromProto $ getKind r,
        rafters     : fromProto <$> getRafters r,
        parameters  : fromProto $ getParams r,
        arrayComps  : fromProto <$> fromMapPB (getComponents r)
    }