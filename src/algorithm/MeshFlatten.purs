module Algorithm.MeshFlatten where

import Prelude

import Algorithm.PointInPolygon (pointInPolygon)
import Data.Array (concatMap, filter, length, range, zip, zipWith)
import Data.Default (class Default, def)
import Data.Foldable (sequence_)
import Data.Lens (Lens', (^.), (.~))
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy(..))
import Data.Tuple (fst, snd)
import Editor.Common.Lenses (_center, _index, _item, _maxX, _maxY, _minX, _minY, _normal, _polygon, _position)
import Effect (Effect)
import Math.Angle (degreeVal)
import Model.Polygon (Polygon, polygonBBox, toPolygon)
import Model.Roof.RoofPlate (RoofPlate)
import RBush.RBush (BBox, RBush, load, mkRBush, search)
import Three.Core.Geometry (class IsBufferGeometry, clone, getAttribute, isBufferAttribute, setNeedsUpdate, setXYZ)
import Three.Core.Mesh (Mesh, setBufferGeometry)
import Three.Math.Vector (Vector2, Vector3, addScaled, angleBetween, mkVec2, vecX, vecY, vecZ, (<->), (<.>))


newtype VertexItem = VertexItem {
    vertex :: Vector3,
    normal :: Vector3,
    index  :: Int
}
derive instance newtypeVertexItem :: Newtype VertexItem _
instance defaultVertexItem :: Default VertexItem where
    def = VertexItem {
        vertex : def,
        normal : def,
        index  : 0
    }

_vertex :: forall t a r. Newtype t { vertex :: a | r } => Lens' t a
_vertex = _Newtype <<< prop (SProxy :: SProxy "vertex" )

-- | offset used to calculate bounding box for a point
vertexOffset :: Number
vertexOffset = 0.0001

-- | calculate VertexItem for a vertex point
vertexItem :: Vector3 -> Vector3 -> Int -> BBox VertexItem
vertexItem point normal index =
    let x = vecX point
        y = vecY point
    in def # _minX .~ x - vertexOffset
           # _minY .~ y - vertexOffset
           # _maxX .~ x + vertexOffset
           # _maxY .~ y + vertexOffset
           # _item .~ (def # _vertex .~ point
                           # _normal .~ normal
                           # _index  .~ index)

-- | build an RTree from a list of vertices
buildRTree :: Array Vector3 -> Array Vector3 -> Effect (RBush VertexItem)
buildRTree vertices normals = do
    let vns     = zip vertices normals
        nums    = range 0 (length vns - 1)
        f t idx = vertexItem (fst t) (snd t) idx
        items   = zipWith f vns nums
    tree <- mkRBush
    load items tree
    pure tree

newtype RoofFlattener = RoofFlattener {
    normal  :: Vector3,
    center  :: Vector3,
    polygon :: Polygon Vector2
}

derive instance newtypeRoofFlattener :: Newtype RoofFlattener _

-- | flatten a vertex, returns a new position for that vertex
flatten :: RoofFlattener -> Vector3 -> Vector3
flatten flattener v = addScaled v (flattener ^. _normal) scale
    where nv = flattener ^. _center <-> v
          scale = flattener ^. _normal <.> nv

-- | calculate distance from the param position to the roof
distToRoof :: RoofFlattener -> Vector3 -> Number
distToRoof flattener v = flattener ^. _normal <.> nv
    where nv = v <-> flattener ^. _center

-- | get the RoofFlattener for a roof
roofFlattener :: RoofPlate -> RoofFlattener
roofFlattener r = RoofFlattener { normal: r ^. _normal, center: r ^. _center, polygon: toPolygon r}

-- | flattened vertex info
newtype FlattenedVertex = FlattenedVertex {
    index    :: Int,
    position :: Vector3
}

derive instance newtypeFlattendVertex :: Newtype FlattenedVertex _

-- | apply the flattened vertices to the BufferGeometry and return a new one
applyFlattenedVertex :: forall geo. IsBufferGeometry geo => geo -> Array FlattenedVertex -> Effect geo
applyFlattenedVertex geo fvs = do
    newGeo <- clone geo
    let attr = getAttribute "position" newGeo
    if isBufferAttribute attr
    then do
        let apply fv = do
                let p = fv ^. _position
                setXYZ (fv ^. _index) (vecX p) (vecY p) (vecZ p) attr
        sequence_ (apply <$> fvs)
        setNeedsUpdate true attr
        pure newGeo
    else pure geo

-- | Flatten a single roof plate
flattenRoofplate :: RBush VertexItem -> RoofPlate -> Array FlattenedVertex
flattenRoofplate tree roof = flattenF <$> filter f candidates
    where flattener = roofFlattener roof
          poly = flattener ^. _polygon
                            
          candidates = search (polygonBBox poly) tree

          -- check if a candidate is under the roof polygon
          pointInRoof c = let v = c ^. _vertex
                              point = mkVec2 (vecX v) (vecY v)
                          in pointInPolygon poly point
                                
          -- check the distance to the roof and angle between its normal
          -- vector with the roof normal vector.
          checkDistAndAngle c = let angle = angleBetween (flattener ^. _normal) (c ^. _normal)
                                    dist = distToRoof flattener (c ^. _vertex)
                                in (dist < 0.5 && dist >= 0.0) || (dist < 0.0 && dist > -1.0 && degreeVal angle < 20.0)
                            
          -- filter function
          f c = pointInRoof c && checkDistAndAngle c
          flattenF c = FlattenedVertex {
              index: c ^. _index,
              position: flatten flattener (c ^. _vertex)
              }


-- | flatten all roofplates
flattenRoofPlates :: forall geo. IsBufferGeometry geo => geo -> RBush VertexItem -> Mesh -> Array RoofPlate -> Effect Unit
flattenRoofPlates geo tree house roofs = do
    let fvs = concatMap (flattenRoofplate tree) roofs
    newGeo <- applyFlattenedVertex geo fvs
    setBufferGeometry newGeo house
