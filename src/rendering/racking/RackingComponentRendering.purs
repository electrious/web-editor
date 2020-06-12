module Rendering.Racking.RackingComponentRendering where

import Prelude hiding (add)

import Model.Racking.RoofRackingData (RackingComp(..))
import Rendering.Racking.BXRendering (BXRackingComponentRenderable(..))
import Rendering.Racking.FXRendering (FXRackingComponentRenderable(..))
import Rendering.Racking.GAFRendering (GAFRackingComponentRenderable(..))
import Rendering.Racking.XRFlatRendering (XRFlatRackingComponentRenderable(..))
import Rendering.Racking.XRRendering (XRRackingComponentRenderable(..))
import Rendering.Renderable (class RenderableWithSlope, render, renderWithSlope)
import Three.Core.Object3D (Object3D, add, mkObject3D, setName)
  
newtype RackignCompRenderable = RackignCompRenderable RackingComp
instance renderableRackingComp :: RenderableWithSlope RackignCompRenderable Object3D where
    renderWithSlope slope (RackignCompRenderable r) = do
        let f (FX fx)     = render $ FXRackingComponentRenderable fx
            f (XR xr)     = render $ XRRackingComponentRenderable xr
            f (XRFlat fl) = renderWithSlope slope $ XRFlatRackingComponentRenderable fl
            f (BX bx)     = render $ BXRackingComponentRenderable bx
            f (GAF gaf)   = render $ GAFRackingComponentRenderable gaf
        c <- mkObject3D
        setName "RackingComp" c
        
        comp :: Object3D <- f r
        add comp c

        pure c
