module API.V1.Racking.Request where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Map (Map)
import Data.Newtype (class Newtype)
import Data.Traversable (traverse)
import Editor.Common.ProtoCodable (class ProtoEncodable, toProto)
import Effect (Effect)
import Model.MapPB (MapPB, toMapPB)
import Model.Racking.RoofParameter (RoofParameter, RoofParameterPB)
import Model.Roof.Panel (Panel, PanelPB)
import Util (ffi, fpi)

foreign import data RackRequestPB :: Type
foreign import mkRackRequestPB :: Effect RackRequestPB

getParams :: RackRequestPB -> MapPB String RoofParameterPB
getParams = ffi ["r"] "r.getParams()"

setParams :: MapPB String RoofParameterPB -> RackRequestPB -> Effect Unit
setParams = fpi ["p", "r", ""] "r.setParams(p)"

getPanels :: RackRequestPB -> Array PanelPB
getPanels = ffi ["r"] "r.getPanelsList()"

setPanels :: Array PanelPB -> RackRequestPB -> Effect Unit
setPanels = fpi ["ps", "r", ""] "r.setPanelsList(ps)"

newtype RackRequest = RackRequest {
    parameters :: Map String RoofParameter,
    panels     :: Array Panel
}

derive instance newtypeRackRequest :: Newtype RackRequest _
derive instance genericRackRequest :: Generic RackRequest _
instance showRackRequest :: Show RackRequest where
    show = genericShow
instance protoEncodableRackRequest :: ProtoEncodable RackRequest RackRequestPB where
    toProto (RackRequest r) = do
        rp <- mkRackRequestPB
        param <- toMapPB =<< traverse toProto r.parameters
        setParams param rp
        ps <- traverse toProto r.panels
        setPanels ps rp
        pure rp
