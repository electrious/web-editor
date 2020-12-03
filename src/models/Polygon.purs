module Model.Polygon where

import Prelude

import Custom.Mesh (TappableMesh(..), mkTappableMesh)
import Data.Default (def)
import Data.Lens (Lens', (^.))
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Newtype (class Newtype)
import Data.Tuple (snd)
import Effect (Effect)
import Effect.Class (liftEffect)
import Rendering.Node (getEnv, leaf, tapMesh)
import Rendering.NodeRenderable (class NodeRenderable)
import Three.Core.Geometry (mkShape, mkShapeGeometry)
import Three.Core.Material (MeshBasicMaterial)
import Three.Math.Vector (Vector2)

newtype Polygon = Polygon (Array Vector2)

derive instance newtypePolygon :: Newtype Polygon _
derive instance eqPolygon :: Eq Polygon

_polyVerts :: Lens' Polygon (Array Vector2)
_polyVerts = _Newtype


instance nodeRenderablePolygon :: NodeRenderable MeshBasicMaterial Polygon TappableMesh where
    render p = do
        mat <- getEnv
        shp <- liftEffect $ mkShape $ p ^. _polyVerts
        geo <- liftEffect $ mkShapeGeometry shp
        snd <$> tapMesh def geo mat leaf


renderPolygon :: Polygon -> MeshBasicMaterial -> Effect TappableMesh
renderPolygon p mat = do
    shp <- mkShape $ p ^. _polyVerts
    geo <- mkShapeGeometry shp
    mkTappableMesh geo mat
