module Model.Polygon (Polygon(..), _polyVerts, newPolygon, polygonAround, numOfVerts,
                      addVertexAt, delVertexAt, updateVertAt, modifyVertAt, polyCenter, polyEdges, polyOutline, polyWindows,
                      polyMidPoints, polygonBBox, counterClockPoly, polyPlane, normalizeContour,
                      renderPolygon, class IsPolygon, toPolygon, PolyOrient(..), polygonOrient) where

import Prelude hiding (add)

import Algorithm.Plane (Plane, mkPlane)
import Control.Monad.Writer (tell)
import Custom.Mesh (TapMouseMesh, TappableMesh, mkTappableMesh)
import Data.Array (cons, deleteAt, fromFoldable, head, init, insertAt, last, length, mapWithIndex, modifyAt, reverse, snoc, tail, updateAt, zip, zipWith, (!!))
import Data.Default (class Default, def)
import Data.Filterable (filter)
import Data.Foldable (class Foldable, foldl, maximum, minimum)
import Data.Generic.Rep (class Generic)
import Data.Int (toNumber)
import Data.Lens (Lens', (^.), (.~))
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Maybe (Maybe, fromMaybe)
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
import Data.Traversable (class Traversable)
import Data.Triple (Triple(..), second)
import Data.Tuple (Tuple(..), uncurry)
import Editor.Common.Lenses (_maxX, _maxY, _mesh, _minX, _minY)
import Editor.Disposable (Disposee(..))
import Effect (Effect)
import Effect.Class (liftEffect)
import FRP.Dynamic (Dynamic, current, subscribeDyn)
import Math.LineSeg (LineSeg, lineVec, mkLineSeg)
import RBush.RBush (BBox)
import Rendering.Node (getEnv, tapMouseMesh)
import Rendering.NodeRenderable (class NodeRenderable)
import Three.Core.Geometry (mkShape, mkShapeGeometry)
import Three.Core.Material (MeshBasicMaterial)
import Three.Core.Mesh (setMaterial)
import Three.Math.Vector (class Vector, cross, dist, getVector, mkVec3, normal, toVec2, updateVector, vecX, vecY, vecZ, (<**>), (<+>), (<->))

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

-- make a polygon in counter clockwise orientation
counterClockPoly :: forall v. Vector v => Polygon v -> Polygon v
counterClockPoly poly = case polygonOrient poly of
    Clockwise -> Polygon $ reverse $ poly ^. _polyVerts
    CounterClockwise -> poly

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

modifyVertAt :: forall v. Int -> (v -> v) -> Polygon v -> Polygon v
modifyVertAt idx f poly@(Polygon pns) = fromMaybe poly $ Polygon <$> modifyAt idx f pns

-- | calculate the center based on polygon
polyCenter :: forall v. Default v => Vector v => Polygon v -> v
polyCenter poly = (foldl (<+>) def vs) <**> (1.0 / l)
    where l  = toNumber $ length vs
          vs = poly ^. _polyVerts


-- | calculate plane for a polygon
polyPlane :: forall v. Default v => Vector v => Polygon v -> Plane
polyPlane poly = mkPlane (getVector $ polyCenter poly) n
    where ls = polyOutline poly
          f  = getVector <<< lineVec
          l1 = f <$> ls !! 0
          l2 = f <$> ls !! 1
          n = fromMaybe def $ map normal $ cross <$> l1 <*> l2

-- | get all edges of the polygon
polyEdges :: forall v. Vector v => Polygon v -> Array (Tuple v v)
polyEdges poly = if length vs < 2
                 then []
                 else zip vs v2Lst
    where vs    = poly ^. _polyVerts
          v2Lst = fromMaybe [] $ snoc <$> tail vs <*> head vs

polyOutline :: forall v. Vector v => Polygon v -> Array (LineSeg v)
polyOutline = map (uncurry mkLineSeg) <<< polyEdges

-- | for each vertex, get a triple with its left and right neighbor vertices
polyWindows :: forall v. Polygon v -> Array (Triple v v v)
polyWindows (Polygon vs) = if length vs < 3
                           then []
                           else zipWith f prevs $ zip vs nexts
    where prevs = fromMaybe vs $ cons <$> last vs <*> init vs
          nexts = fromMaybe vs $ snoc <$> tail vs <*> head vs

          f p (Tuple v n) = Triple p v n

-- delete duplicated vertices or connect two consecutive edges if they're in the same direction
normalizeContour :: forall a v. Eq v => Vector v => (a -> v) -> Polygon a -> Polygon a
normalizeContour vf = newPolygon <<< map second <<< filter f <<< polyWindows
    where f (Triple prev p next) = 
              let prevN = vf prev
                  pN    = vf p
                  nextN = vf next
              in not $ pN == nextN || normal (pN <-> prevN) == normal (nextN <-> pN)

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


data PolyOrient = Clockwise
                | CounterClockwise

derive instance eqPolyOrient :: Eq PolyOrient
derive instance ordPolyOrient :: Ord PolyOrient
derive instance genericPolyOrient :: Generic PolyOrient _
instance showPolyOrient :: Show PolyOrient where
    show = genericShow


-- get polygon orient for a 2d polygon
polygonOrient :: forall v. Vector v => Polygon v -> PolyOrient
polygonOrient poly = toOrient $ foldl f 0.0 $ polyEdges poly
    where f n (Tuple s e) = n + (vecX e - vecX s) * (vecY s + vecY e)
          toOrient v | v >= 0.0  = Clockwise
                     | otherwise = CounterClockwise
