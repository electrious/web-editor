module Algorithm.MeshFlatten where

import Prelude

import Algorithm.PointInPolygon (pointInPolygon)
import Data.Array (concat, filter, length, range, zip, zipWith)
import Data.Foldable (maximum, minimum, sequence_)
import Data.Lens (Lens', (^.))
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Maybe (fromMaybe)
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy(..))
import Data.Traversable (traverse)
import Data.Tuple (fst, snd)
import Effect (Effect)
import Math.Angle (degreeVal)
import Models.RoofPlate (Polygon, RoofPlate, _center, _normal, angleBetween, getRoofPolygon)
import RBush.RBush (BBox, RBush, load, mkRBush, search)
import Three.Core.Geometry (BufferGeometry, clone, getAttribute, isBufferAttribute, setNeedsUpdate, setXYZ)
import Three.Core.Mesh (Mesh, setBufferGeometry)
import Three.Math.Vector (Vector3, addScaled, mkVec2, vecX, vecY, vecZ, (<->), (<.>))

-- | vertex data that will be inserted into RTree
type VertexItem = BBox (vertex :: Vector3, normal :: Vector3, index:: Int)

-- | offset used to calculate bounding box for a point
vertexOffset :: Number
vertexOffset = 0.0001

-- | calculate VertexItem for a vertex point
vertexItem :: Vector3 -> Vector3 -> Int -> VertexItem
vertexItem point normal index =
    let x = vecX point
        y = vecY point
    in {
        minX   : x - vertexOffset,
        minY   : y - vertexOffset,
        maxX   : x + vertexOffset,
        maxY   : y + vertexOffset,
        vertex : point,
        normal : normal,
        index  : index
    }

-- | build an RTree from a list of vertices
buildRTree :: Array Vector3 -> Array Vector3 -> Effect (RBush VertexItem)
buildRTree vertices normals = do
    let vns = zip vertices normals
        nums = range 0 (length vns - 1)
        f t idx = vertexItem (fst t) (snd t) idx
        items = zipWith f vns nums
    tree <- mkRBush
    load items tree
    pure tree

-- | get the bounding box of a polygon
polygonBoundingBox :: Polygon -> BBox ()
polygonBoundingBox polygon = { minX: minX, minY: minY, maxX: maxX, maxY: maxY}
    where xs = vecX <$> polygon
          ys = vecY <$> polygon
          minX = fromMaybe 0.0 (minimum xs)
          minY = fromMaybe 0.0 (minimum ys)
          maxX = fromMaybe 0.0 (maximum xs)
          maxY = fromMaybe 0.0 (maximum ys)

newtype RoofFlattener = RoofFlattener {
    roofNormal :: Vector3,
    roofCenter :: Vector3,
    roofPolygon :: Polygon
}

derive instance newtypeRoofFlattener :: Newtype RoofFlattener _

_roofNormal :: Lens' RoofFlattener Vector3
_roofNormal = _Newtype <<< prop (SProxy :: SProxy "roofNormal")

_roofCenter :: Lens' RoofFlattener Vector3
_roofCenter = _Newtype <<< prop (SProxy :: SProxy "roofCenter")

_roofPolygon :: Lens' RoofFlattener Polygon
_roofPolygon = _Newtype <<< prop (SProxy :: SProxy "roofPolygon")

-- | flatten a vertex, returns a new position for that vertex
flatten :: RoofFlattener -> Vector3 -> Vector3
flatten flattener v = addScaled v (flattener ^. _roofNormal) scale
    where nv = flattener ^. _roofCenter <-> v
          scale = flattener ^. _roofNormal <.> nv

-- | calculate distance from the param position to the roof
distToRoof :: RoofFlattener -> Vector3 -> Number
distToRoof flattener v = flattener ^. _roofNormal <.> nv
    where nv = v <-> flattener ^. _roofCenter

-- | get the RoofFlattener for a roof
roofFlattener :: RoofPlate -> RoofFlattener
roofFlattener r = RoofFlattener { roofNormal: r ^. _normal, roofCenter: r ^. _center, roofPolygon: getRoofPolygon r}

-- | flattened vertex info
newtype FlattenedVertex = FlattenedVertex {
    index  :: Int,
    newPos :: Vector3
}

derive instance newtypeFlattendVertex :: Newtype FlattenedVertex _

_index :: Lens' FlattenedVertex Int
_index = _Newtype <<< prop (SProxy :: SProxy "index")

_newPos :: Lens' FlattenedVertex Vector3
_newPos = _Newtype <<< prop (SProxy :: SProxy "newPos")

-- | apply the flattened vertices to the BufferGeometry and return a new one
applyFlattenedVertex :: forall geo. BufferGeometry geo -> Array FlattenedVertex -> Effect (BufferGeometry geo)
applyFlattenedVertex geo fvs = do
    newGeo <- clone geo
    let attr = getAttribute "position" newGeo
    if isBufferAttribute attr
    then do
        let apply fv = do
                let p = fv ^. _newPos
                setXYZ (fv ^. _index) (vecX p) (vecY p) (vecZ p) attr
        sequence_ (apply <$> fvs)
        setNeedsUpdate true attr
        pure newGeo
    else pure geo

-- | Flatten a single roof plate
flattenRoofplate :: RBush VertexItem -> RoofPlate -> Effect (Array FlattenedVertex)
flattenRoofplate tree roof = do
    let flattener = roofFlattener roof
        poly = flattener ^. _roofPolygon
    
    candidates <- search (polygonBoundingBox poly) tree

    -- check if a candidate is under the roof polygon
    let pointInRoof c = let v = c.vertex
                            point = mkVec2 (vecX v) (vecY v)
                        in pointInPolygon poly point
        
        -- check the distance to the roof and angle between its normal
        -- vector with the roof normal vector.
        checkDistAndAngle c = let angle = angleBetween (flattener ^. _roofNormal) c.normal
                                  dist = distToRoof flattener c.vertex
                              in (dist < 0.5 && dist >= 0.0) || (dist < 0.0 && dist > -1.0 && degreeVal angle < 20.0)
    
        -- filter function
        f c = pointInRoof c && checkDistAndAngle c
        flattenF c = FlattenedVertex { index: c.index, newPos: flatten flattener c.vertex }
    
    pure $ flattenF <$> filter f candidates


-- | flatten all roofplates
flattenRoofPlates :: forall a geo. BufferGeometry geo -> RBush VertexItem -> Mesh a -> Array RoofPlate -> Effect Unit
flattenRoofPlates geo tree house roofs = do
    fvs <- concat <$> traverse (flattenRoofplate tree) roofs
    newGeo <- applyFlattenedVertex geo fvs
    setBufferGeometry newGeo house
