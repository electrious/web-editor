module Editor.Polygon where

import Prelude

import Custom.Mesh (TappableMesh, mkTappableMesh)
import Data.Lens ((^.), Lens')
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Newtype (class Newtype)
import Effect (Effect)
import Three.Core.Geometry (mkShape, mkShapeGeometry)
import Three.Core.Material (MeshBasicMaterial)
import Three.Math.Vector (Vector2)

newtype Polygon = Polygon (Array Vector2)

derive instance newtypePolygon :: Newtype Polygon _

_polyVerts :: Lens' Polygon (Array Vector2)
_polyVerts = _Newtype

renderPolygon :: Polygon -> MeshBasicMaterial -> Effect TappableMesh
renderPolygon p mat = do
    shp <- mkShape $ p ^. _polyVerts
    geo <- mkShapeGeometry shp
    mkTappableMesh geo mat
