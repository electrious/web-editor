module Rendering.Racking.RackingComponentRendering where

import Prelude hiding (add)

import Data.Default (def)
import Data.Lens ((.~))
import Editor.Common.Lenses (_name)
import Math.Angle (Angle)
import Model.Racking.RoofRackingData (RackingComp(..))
import Taihe.Node (Node, node)
import Rendering.Racking.BXRendering (renderBX)
import Rendering.Racking.FXRendering (renderFX)
import Rendering.Racking.GAFRendering (renderGAF)
import Rendering.Racking.XRFlatRendering (renderXRFlat)
import Rendering.Racking.XRRendering (renderXR)
  

renderRackingComp :: forall e. Angle -> RackingComp -> Node e Unit
renderRackingComp slope r = node (def # _name .~ "RackingComp") $
    case r of
        FX fx     -> renderFX fx
        XR xr     -> renderXR xr
        XRFlat fl -> renderXRFlat slope fl
        BX bx     -> renderBX bx
        GAF gaf   -> renderGAF gaf
