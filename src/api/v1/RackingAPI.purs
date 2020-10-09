module API.V1.RackingAPI (updateRacking, RackingAPIClient, mkRackingAPIClient) where

import Prelude

import API.Error (ErrorPB)
import API.V1.Racking.Request (RackRequest, RackRequestPB)
import Editor.Common.ProtoCodable (fromProto, toProto)
import Effect (Effect)
import FRP.Event (Event, makeEvent)
import Model.Racking.RackingSystem (RackingSystem, RackingSystemPB)
import Util (ffi, fpi)

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

-- | Call GRPC API to update racking data
updateRacking :: RackRequest -> RackingAPIClient -> Effect (Event RackingSystem)
updateRacking req client = do
    reqPb <- toProto req
    dReq <- mkDoRackRequestPB
    setRequest reqPb dReq
    pure $ makeEvent \k -> do
        doRack dReq (\err resp -> k (fromProto $ getRacking resp)) client
        pure (pure unit)