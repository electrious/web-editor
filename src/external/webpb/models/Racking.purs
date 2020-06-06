module WebPB.Models.Racking where

import Prelude

import Effect (Effect)
import Util (ffi, fpi)
import WebPB.Map (MapPB)
import WebPB.Models.Racking.Component (ComponentPB)
import WebPB.Models.Racking.Kind (RackingKind)
import WebPB.Models.Racking.Rafter (RafterPB)
import WebPB.Models.Racking.RoofParameter (RoofParameterPB)

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

foreign import data RackingSystemPB :: Type
foreign import mkRackingSystemPB :: Effect RackingSystemPB

getRoofRackings :: RackingSystemPB -> MapPB String RoofRackingResultPB
getRoofRackings = ffi ["r"] "r.getRoofRackings()"

setRoofRackings :: MapPB String RoofRackingResultPB -> RackingSystemPB -> Effect Unit
setRoofRackings = fpi ["rr", "r", ""] "r.setRoofRackings(rr)"
