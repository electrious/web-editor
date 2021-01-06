module Model.Polygon where

import Prelude

import Control.Monad.Writer (tell)
import Custom.Mesh (TapMouseMesh, TappableMesh, mkTappableMesh)
import Data.Array (fromFoldable, length)
import Data.Default (class Default, def)
import Data.Foldable (class Foldable)
import Data.Lens (Lens', (^.))
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Newtype (class Newtype)
import Editor.Common.Lenses (_mesh)
import Editor.Disposable (Disposee(..))
import Effect (Effect)
import Effect.Class (liftEffect)
import FRP.Dynamic (Dynamic, current, subscribeDyn)
import Rendering.Node (getEnv, tapMouseMesh)
import Rendering.NodeRenderable (class NodeRenderable)
import Three.Core.Geometry (mkShape, mkShapeGeometry)
import Three.Core.Material (MeshBasicMaterial)
import Three.Core.Mesh (setMaterial)
import Three.Math.Vector (class IsVector2, Vector2, mkVec2, toVec2, vecX, vecY)

newtype Polygon v = Polygon (Array v)

derive instance newtypePolygon :: Newtype (Polygon v) _
derive instance eqPolygon :: Eq v => Eq (Polygon v)
instance defaultPolygon :: Default (Polygon v) where
    def = Polygon []

_polyVerts :: forall v. Lens' (Polygon v) (Array v)
_polyVerts = _Newtype

-- | create a Polygon from a list of Vectors
newPolygon :: forall f v. Foldable f => f v -> Polygon v
newPolygon = fromFoldable >>> Polygon

-- | create a Polygon around a central vector
polygonAround :: Vector2 -> Polygon Vector2
polygonAround p = newPolygon [p1, p2, p3, p4]
    where x = vecX p
          y = vecY p
          l = 10.0
          p1 = mkVec2 (x - l) (y - l)
          p2 = mkVec2 (x - l) (y + l)
          p3 = mkVec2 (x + l) (y + l)
          p4 = mkVec2 (x + l) (y - l)

numOfVerts :: forall v. Polygon v -> Int
numOfVerts (Polygon vs) = length vs

instance nodeRenderablePolygon :: IsVector2 v => NodeRenderable (Dynamic MeshBasicMaterial) (Polygon v) TapMouseMesh where
    render p = do
        -- get the current material used
        matDyn <- getEnv
        mat <- liftEffect $ current matDyn
        
        shp <- liftEffect $ mkShape $ toVec2 <$> p ^. _polyVerts
        geo <- liftEffect $ mkShapeGeometry shp
        
        m <- tapMouseMesh def geo mat

        -- update the material when it changes
        let mesh = m ^. _mesh
        d <- liftEffect $ subscribeDyn matDyn (flip setMaterial mesh)
        tell $ Disposee d

        pure m


renderPolygon :: forall v. IsVector2 v => Polygon v -> MeshBasicMaterial -> Effect TappableMesh
renderPolygon p mat = do
    shp <- mkShape $ toVec2 <$> p ^. _polyVerts
    geo <- mkShapeGeometry shp
    mkTappableMesh geo mat


class IsPolygon p v where
    toPolygon :: p -> Polygon v
