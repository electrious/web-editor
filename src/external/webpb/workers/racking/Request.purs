module WebPB.Workers.Racking.Request where

import Prelude

import Effect (Effect)
import Util (ffi, fpi)
import WebPB.Map (MapPB)
import WebPB.Models.Racking.RoofParameter (RoofParameterPB)
import WebPB.Models.Roofplate.Panel (PanelPB)

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
