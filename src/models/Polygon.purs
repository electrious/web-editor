module Model.Polygon (Polygon, _polyVerts, newPolygon, polygonAround, numOfVerts,
                      addVertexAt, delVertexAt, updateVertAt, polyCenter, polyEdges, polyMidPoints, polygonBBox,
                      renderPolygon, class IsPolygon, toPolygon) where

import Prelude hiding (add)

import Control.Monad.Writer (tell)
import Custom.Mesh (TapMouseMesh, TappableMesh, mkTappableMesh)
import Data.Array (deleteAt, fromFoldable, head, insertAt, length, mapWithIndex, snoc, tail, updateAt, zip, zipWith)
import Data.Default (class Default, def)
import Data.Filterable (filter)
import Data.Foldable (class Foldable, foldl, maximum, minimum)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Int (toNumber)
import Data.Lens (Lens', (^.), (.~))
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Maybe (Maybe, fromMaybe)
import Data.Newtype (class Newtype)
import Data.Traversable (class Traversable)
import Data.Tuple (Tuple(..))
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
import Three.Math.Vector (class Vector, dist, getVector, mkVec3, toVec2, updateVector, vecX, vecY, vecZ, (<**>), (<+>))

newtype Polygon v = Polygon (Array v)

derive instance genericPolygon :: Generic (Polygon v) _
derive instance newtypePolygon :: Newtype (Polygon v) _
derive instance eqPolygon :: Eq v => Eq (Polygon v)
derive newtype instance functorPolygon :: Functor Polygon
derive newtype instance foldablePolygon :: Foldable Polygon
derive newtype instance traversablePolygon :: Traversable Polygon
instance showPolygon :: Show v => Show (Polygon v) where
    show = genericShow
instance defaultPolygon :: Default (Polygon v) where
    def = Polygon []

_polyVerts :: forall v. Lens' (Polygon v) (Array v)
_polyVerts = _Newtype

-- | create a Polygon from a list of Vectors
newPolygon :: forall f v. Foldable f => f v -> Polygon v
newPolygon = fromFoldable >>> Polygon

-- | create a Polygon around a central vector
polygonAround :: forall v. Vector v => Number -> v -> Polygon v
polygonAround l p = newPolygon $ updateVector p <$> [p1, p2, p3, p4]
    where x = vecX p
          y = vecY p
          v = getVector p
          z = vecZ v
          
          p1 = mkVec3 (x - l) (y - l) z
          p2 = mkVec3 (x - l) (y + l) z
          p3 = mkVec3 (x + l) (y + l) z
          p4 = mkVec3 (x + l) (y - l) z

numOfVerts :: forall v. Polygon v -> Int
numOfVerts (Polygon vs) = length vs

addVertexAt :: forall v. Int -> v -> Polygon v -> Maybe (Polygon v)
addVertexAt idx p (Polygon pns) = Polygon <$> insertAt idx p pns

delVertexAt :: forall v. Int -> Polygon v -> Polygon v
delVertexAt idx poly = if numOfVerts poly > 3
                       then Polygon $ fromMaybe [] (deleteAt idx $ poly ^. _polyVerts)
                       else poly

updateVertAt :: forall v. Int -> v -> Polygon v -> Polygon v
updateVertAt idx p poly@(Polygon pns) = fromMaybe poly $ Polygon <$> updateAt idx p pns
    
-- | calculate the center based on polygon
polyCenter :: forall v. Default v => Vector v => Polygon v -> v
polyCenter poly = (foldl (<+>) def vs) <**> (1.0 / l)
    where l  = toNumber $ length vs
          vs = poly ^. _polyVerts


-- | get all edges of the polygon
polyEdges :: forall v. Vector v => Polygon v -> Array (Tuple v v)
polyEdges poly = if length vs < 2
                 then []
                 else zip vs v2Lst
    where vs    = poly ^. _polyVerts
          v2Lst = fromMaybe [] $ snoc <$> tail vs <*> head vs


-- | calculate all middle points on all edges of a polygon
polyMidPoints :: forall v. Vector v => Polygon v -> Array (Tuple Int v)
polyMidPoints poly = if length vs < 2
                     then []
                     else toResult <$> filter validDist (zipWith f v1Lst v2Lst)
    where vs = poly ^. _polyVerts
          v1Lst = mapWithIndex Tuple vs
          v2Lst = fromMaybe [] $ snoc <$> (tail vs) <*> (head vs)

          validDist p = p.dist > 1.0
          toResult  p = Tuple p.index p.position

          f (Tuple idx v1) v2 = {
              dist     : dist v1 v2,
              position : ((v1 <+> v2) <**> 0.5),
              index    : idx + 1
          }


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

instance isPolygonBase :: IsPolygon (Polygon v) v where
    toPolygon = identity
