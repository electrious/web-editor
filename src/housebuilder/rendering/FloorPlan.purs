module HouseBuilder.Rendering.FloorPlan where

import Data.Lens ((^.))
import Custom.Mesh (TappableMesh(..))
import Editor.Common.Lenses (_polygon)
import Model.HouseBuilder.FloorPlan (FloorPlan)
import Rendering.NodeRenderable (class NodeRenderable, render)
import Three.Core.Material (MeshBasicMaterial)


newtype FloorPlanRenderable = FloorPlanRenderable FloorPlan

instance nodeRenderableFloorPlan :: NodeRenderable MeshBasicMaterial FloorPlanRenderable TappableMesh where
    render (FloorPlanRenderable fp) = render (fp ^. _polygon)
