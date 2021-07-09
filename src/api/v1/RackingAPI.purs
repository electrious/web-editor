module API.V1.RackingAPI (updateRacking, RackingAPIClient, mkRackingAPIClient) where

import Prelude

import API.Error (ErrorPB)
import API.V1.Racking.Request (RackRequest, RackRequestPB)
import Editor.Common.ProtoCodable (fromProto, toProto)
import Effect (Effect)
import FRP.Event (Event, makeEvent)
import Model.Racking.RackingSystem (RackingSystem, RackingSystemPB)

foreign import data DoRackRequestPB :: Type
foreign import mkDoRackRequestPB :: Effect DoRackRequestPB

foreign import getRequest :: DoRackRequestPB -> RackRequestPB
foreign import setRequest :: RackRequestPB -> DoRackRequestPB -> Effect Unit

foreign import data DoRackResponsePB :: Type
foreign import mkDoRackResponsePB :: Effect DoRackResponsePB

foreign import getRacking :: DoRackResponsePB -> RackingSystemPB
foreign import setRacking :: RackingSystemPB -> DoRackResponsePB -> Effect Unit

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
        doRack dReq (\_ resp -> k (fromProto $ getRacking resp)) client
        pure (pure unit)
