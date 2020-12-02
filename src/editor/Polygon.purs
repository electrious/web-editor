module Editor.Polygon where

import Prelude

import Custom.Mesh (TappableMesh, mkTappableMesh)
import Data.Default (def)
import Data.Lens ((^.))
import Data.Tuple (snd)
import Effect (Effect)
import Effect.Class (liftEffect)
import Model.Polygon (Polygon, _polyVerts)
import Rendering.Node (getEnv, leaf, tapMesh)
import Rendering.NodeRenderable (class NodeRenderable)
import Three.Core.Geometry (mkShape, mkShapeGeometry)
import Three.Core.Material (MeshBasicMaterial)


renderPolygon :: Polygon -> MeshBasicMaterial -> Effect TappableMesh
renderPolygon p mat = do
    shp <- mkShape $ p ^. _polyVerts
    geo <- mkShapeGeometry shp
    mkTappableMesh geo mat


newtype PolygonRenderable = PolygonRenderable Polygon

instance nodeRenderablePolygon :: NodeRenderable MeshBasicMaterial PolygonRenderable TappableMesh where
    render (PolygonRenderable p) = do
        mat <- getEnv
        shp <- liftEffect $ mkShape $ p ^. _polyVerts
        geo <- liftEffect $ mkShapeGeometry shp
        snd <$> tapMesh def geo mat leaf
