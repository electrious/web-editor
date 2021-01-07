module Model.Polygon (Polygon, _polyVerts, newPolygon, polygonAround, numOfVerts,
                      addVertexAt, delVertexAt, polyCenter, polygonBBox,
                      renderPolygon, class IsPolygon, class PolyVertex,
                      toPolygon, updatePos) where

import Prelude

import Control.Monad.Writer (tell)
import Custom.Mesh (TapMouseMesh, TappableMesh, mkTappableMesh)
import Data.Array (deleteAt, fromFoldable, insertAt, length)
import Data.Default (class Default, def)
import Data.Foldable (class Foldable, foldl, maximum, minimum)
import Data.Int (toNumber)
import Data.Lens (Lens', (^.), (.~))
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Maybe (Maybe, fromMaybe)
import Data.Newtype (class Newtype)
import Editor.Common.Lenses (_maxX, _maxY, _mesh, _minX, _minY)
import Editor.Disposable (Disposee(..))
import Effect (Effect)
import Effect.Class (liftEffect)
import FRP.Dynamic (Dynamic, current, subscribeDyn)
import RBush.RBush (BBox)
import Rendering.Node (getEnv, tapMouseMesh)
import Rendering.NodeRenderable (class NodeRenderable)
import Three.Core.Geometry (mkShape, mkShapeGeometry)
import Three.Core.Material (MeshBasicMaterial)
import Three.Core.Mesh (setMaterial)
import Three.Math.Vector (class Vector, Vector2, Vector3, mkVec2, toVec2, vecX, vecY, (<+>), (<**>))

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

addVertexAt :: forall v. Int -> v -> Polygon v -> Maybe (Polygon v)
addVertexAt idx p (Polygon pns) = Polygon <$> insertAt idx p pns

delVertexAt :: forall v. Int -> Polygon v -> Polygon v
delVertexAt idx poly = if numOfVerts poly > 3
                       then Polygon $ fromMaybe [] (deleteAt idx $ poly ^. _polyVerts)
                       else poly

-- | calculate the center based on polygon
polyCenter :: forall v. Vector v => Default v => Polygon v -> v
polyCenter poly = (foldl (<+>) def vs) <**> (1.0 / l)
    where l  = toNumber $ length vs
          vs = poly ^. _polyVerts

-- | get the bounding box of a polygon
polygonBBox :: forall v. Vector v => Polygon v -> BBox Unit
polygonBBox poly = def # _minX .~ fromMaybe 0.0 (minimum xs)
                       # _minY .~ fromMaybe 0.0 (minimum ys)
                       # _maxX .~ fromMaybe 0.0 (maximum xs)
                       # _maxY .~ fromMaybe 0.0 (maximum ys)
    where vs = poly ^. _polyVerts
          xs = vecX <$> vs
          ys = vecY <$> vs


instance nodeRenderablePolygon :: Vector v => NodeRenderable (Dynamic MeshBasicMaterial) (Polygon v) TapMouseMesh where
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


renderPolygon :: forall v. Vector v => Polygon v -> MeshBasicMaterial -> Effect TappableMesh
renderPolygon p mat = do
    shp <- mkShape $ toVec2 <$> p ^. _polyVerts
    geo <- mkShapeGeometry shp
    mkTappableMesh geo mat


class IsPolygon p v where
    toPolygon :: p -> Polygon v


class (Vector v, Default v) <= PolyVertex v where
    updatePos :: v -> Vector3 -> v


instance polyVertexVector2 :: PolyVertex Vector2 where
    updatePos _ nv = toVec2 nv

instance polyVertexVector3 :: PolyVertex Vector3 where
    updatePos _ nv = nv

