module Editor.Common.ProtoCodable where

import Prelude

import Data.Maybe (fromMaybe)
import Data.UUID (UUID, emptyUUID, parseUUID, toString)
import Effect (Effect)
import Model.UUID (PBUUID, getUUIDString, mkPBUUID, setUUIDString)

class ProtoEncodable msg msgPb | msg -> msgPb where
    toProto :: msg -> Effect msgPb

class ProtoDecodable msg msgPb | msgPb -> msg where
    fromProto :: msgPb -> msg

class (ProtoEncodable msg msgPb, ProtoDecodable msg msgPb) <= ProtoCodable msg msgPb | msg -> msgPb, msgPb -> msg

instance protoEncodableStringUUID :: ProtoEncodable UUID PBUUID where
    toProto s = do
        u <- mkPBUUID
        setUUIDString (toString s) u
        pure u
instance protoDecodableStringUUID :: ProtoDecodable UUID PBUUID where
    fromProto = getUUIDString >>> parseUUID >>> fromMaybe emptyUUID
