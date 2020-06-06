module WebPB.API.V1.RackingAPI where

import Prelude

import Effect (Effect)
import Util (ffi, fpi)
import WebPB.API.Error (ErrorPB)
import WebPB.Models.Racking (RackingSystemPB)
import WebPB.Workers.Racking.Request (RackRequestPB)

foreign import data DoRackRequestPB :: Type
foreign import mkDoRackRequestPB :: Effect DoRackRequestPB

getRequest :: DoRackRequestPB -> RackRequestPB
getRequest = ffi ["r"] "r.getRequest()"

setRequest :: RackRequestPB -> DoRackRequestPB -> Effect Unit
setRequest = fpi ["rq", "r", ""] "r.setRequest(rq)"

foreign import data DoRackResponsePB :: Type
foreign import mkDoRackResponsePB :: Effect DoRackResponsePB

getRacking :: DoRackResponsePB -> RackingSystemPB
getRacking = ffi ["r"] "r.getRacking()"

setRacking :: RackingSystemPB -> DoRackResponsePB -> Effect Unit
setRacking = fpi ["rs", "r", ""] "r.setRacking(rs)"


foreign import data RackingAPIClient :: Type
foreign import mkRackingAPIClient :: String -> Effect RackingAPIClient

foreign import doRack :: DoRackRequestPB -> (ErrorPB -> DoRackResponsePB -> Effect Unit) -> RackingAPIClient -> Effect Unit
