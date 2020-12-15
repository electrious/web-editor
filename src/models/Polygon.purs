module Model.Polygon where

import Prelude

import Control.Monad.Writer (tell)
import Custom.Mesh (TappableMesh, mkTappableMesh)
import Data.Array (fromFoldable)
import Data.Default (class Default, def)
import Data.Foldable (class Foldable)
import Data.Lens (Lens', (^.))
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Newtype (class Newtype)
import Data.Tuple (snd)
import Editor.Common.Lenses (_mesh)
import Editor.Disposable (Disposee(..))
import Effect (Effect)
import Effect.Class (liftEffect)
import FRP.Dynamic (Dynamic, current, subscribeDyn)
import Rendering.Node (getEnv, leaf, tapMesh)
import Rendering.NodeRenderable (class NodeRenderable)
import Three.Core.Geometry (mkShape, mkShapeGeometry)
import Three.Core.Material (MeshBasicMaterial)
import Three.Core.Mesh (setMaterial)
import Three.Math.Vector (Vector2, mkVec2, vecX, vecY)

newtype Polygon = Polygon (Array Vector2)

derive instance newtypePolygon :: Newtype Polygon _
derive instance eqPolygon :: Eq Polygon
instance defaultPolygon :: Default Polygon where
    def = Polygon []

_polyVerts :: Lens' Polygon (Array Vector2)
_polyVerts = _Newtype

-- | create a Polygon from a list of Vectors
newPolygon :: forall f. Foldable f => f Vector2 -> Polygon
newPolygon = fromFoldable >>> Polygon

-- | create a Polygon around a central vector
polygonAround :: Vector2 -> Polygon
polygonAround p = newPolygon [p1, p2, p3, p4, p1]
    where x = vecX p
          y = vecY p
          l = 1.0
          p1 = mkVec2 (x - l) (y - l)
          p2 = mkVec2 (x - l) (y + l)
          p3 = mkVec2 (x + l) (y + l)
          p4 = mkVec2 (x + l) (y - l)


instance nodeRenderablePolygon :: NodeRenderable (Dynamic MeshBasicMaterial) Polygon TappableMesh where
    render p = do
        -- get the current material used
        matDyn <- getEnv
        mat <- liftEffect $ current matDyn
        
        shp <- liftEffect $ mkShape $ p ^. _polyVerts
        geo <- liftEffect $ mkShapeGeometry shp
        
        m <- snd <$> tapMesh def geo mat leaf

        -- update the material when it changes
        let mesh = m ^. _mesh
        d <- liftEffect $ subscribeDyn matDyn (flip setMaterial mesh)
        tell $ Disposee d

        pure m


renderPolygon :: Polygon -> MeshBasicMaterial -> Effect TappableMesh
renderPolygon p mat = do
    shp <- mkShape $ p ^. _polyVerts
    geo <- mkShapeGeometry shp
    mkTappableMesh geo mat
