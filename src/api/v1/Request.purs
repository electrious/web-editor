module API.V1.Racking.Request where

import Prelude

import Effect (Effect)
import Model.MapPB (MapPB)
import Model.Racking.RoofParameter (RoofParameterPB)
import Model.Roof.Panel (PanelPB)
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
