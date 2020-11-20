module Editor.Polygon where

import Prelude

import Custom.Mesh (TappableMesh, mkTappableMesh)
import Data.Lens ((^.))
import Effect (Effect)
import Model.Polygon (Polygon, _polyVerts)
import Three.Core.Geometry (mkShape, mkShapeGeometry)
import Three.Core.Material (MeshBasicMaterial)


renderPolygon :: Polygon -> MeshBasicMaterial -> Effect TappableMesh
renderPolygon p mat = do
    shp <- mkShape $ p ^. _polyVerts
    geo <- mkShapeGeometry shp
    mkTappableMesh geo mat
