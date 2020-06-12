module Rendering.Racking.GAFRendering where

import Prelude
import Model.Racking.GAF.GAFRackingComponent (GAFRackingComponent)
import Rendering.Renderable (class Renderable)
import Three.Core.Object3D (Object3D, mkObject3D, setName)

newtype GAFRackingComponentRenderable = GAFRackingComponentRenderable GAFRackingComponent
instance renderableGAFRackingComponent :: Renderable GAFRackingComponentRenderable Object3D where
    render (GAFRackingComponentRenderable g) = do
        comp <- mkObject3D
        setName "GAFRackingComponent" comp
        pure comp
