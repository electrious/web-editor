module Algorithm.HeatmapMesh where

import Prelude

import Algorithm.Delaunay.Delaunay (triangulate)
import Algorithm.Delaunay.Triangle (Triangle, mkTriangle, triVertex1, triVertex2, triVertex3)
import Algorithm.Delaunay.Vertex (class Vertex)
import Data.Array (foldl, length, (!!))
import Data.Compactable (compact)
import Data.FunctorWithIndex (class FunctorWithIndex, mapWithIndex)
import Data.Generic.Rep (class Generic)
import Data.Ord.Generic (genericCompare)
import Data.Lens (Lens', view, (^.))
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Newtype (class Newtype)
import Data.Number (infinity)
import Data.Symbol (SProxy(..))
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Editor.Common.Lenses (_index, _position, _x, _y)
import Effect (Effect)
import Model.ShadePoint (ShadePoint, _intensity, mkShadePoint)
import Three.Core.Face3 (Face3, indexA, indexB, indexC, mkFace3)
import Three.Core.Geometry (BufferGeometry, mkBufferAttribute, mkBufferGeometry, setAttribute, setIndex)
import Three.Core.TypedArray (face3Array, vector2Array, vector3Array)
import Three.Math.Vector (Vector2, Vector3, mkVec2, mkVec3, vecX, vecY, (<->))
import Three.Math.Vector as Vec

data VertexType = VertPolygon
                | VertShade

derive instance genericVertexType :: Generic VertexType _
derive instance eqVertexType :: Eq VertexType
derive instance ordVertexType :: Ord VertexType

newtype IndexedVertex = IndexedVertex {
    x        :: Number,
    y        :: Number,
    index    :: Int,
    vertType :: VertexType
}

derive instance newtypeIndexedVertex :: Newtype IndexedVertex _
derive instance genericIndexedVertex :: Generic IndexedVertex _
derive instance eqIndexedVertex :: Eq IndexedVertex
instance ordIndexedVertex :: Ord IndexedVertex where
    compare = genericCompare

instance vertexIndexedVertex :: Vertex IndexedVertex where
    vertX = view _x
    vertY = view _y

_vertType :: forall t a r. Newtype t { vertType :: a | r } => Lens' t a
_vertType = _Newtype <<< prop (SProxy :: SProxy "vertType")

mkIndexedVertex :: Number -> Number -> Int -> VertexType -> IndexedVertex
mkIndexedVertex x y i t = IndexedVertex { x : x, y : y, index : i, vertType : t }

-- Convert an array of Vector3 into an array of IndexedVertex values
indexedVertices :: forall f. FunctorWithIndex Int f => f Vector3 -> f IndexedVertex
indexedVertices = mapWithIndex (\i v -> mkIndexedVertex (vecX v) (vecY v) i VertPolygon)

indexedVertsForShadePoints :: forall f. FunctorWithIndex Int f => f ShadePoint -> f IndexedVertex
indexedVertsForShadePoints = mapWithIndex (\i sp -> let pos = sp ^. _position
                                                    in mkIndexedVertex (vecX pos) (vecY pos) i VertShade
                                          )

-- convert vertices and Faces to a list of triangle values
getTriangles :: Array IndexedVertex -> Array Face3 -> Array (Triangle IndexedVertex)
getTriangles vs = compact <<< map mkT
    where mkT f = let v1 = vs !! indexA f
                      v2 = vs !! indexB f
                      v3 = vs !! indexC f
                  in mkTriangle <$> v1 <*> v2 <*> v3

-- convert a roofplate polygon vertex to a SHadePoint based on known shade point values
shadePointForVert :: Array ShadePoint -> Vector3 -> ShadePoint
shadePointForVert ss v = mkShadePoint v i
    where f (Tuple i dist) p = let newD = Vec.length (v <-> p ^. _position)
                               in if newD < dist
                                  then Tuple (p ^. _intensity) newD
                                  else Tuple i dist
          Tuple i _ = foldl f (Tuple 0.0 infinity) ss

-- convert all roofplate polygon vertices to ShadePoint
shadePointsForVerts :: Array ShadePoint -> Array Vector3 -> Array ShadePoint
shadePointsForVerts ss = map (shadePointForVert ss)

-- calculate UV values for all shadepoint
heatmapUVs :: forall f. Functor f => f ShadePoint -> f Vector2
heatmapUVs = map (\sp -> let x = clamp 0.0 1.0 (sp ^. _intensity * 0.8 + 0.2)
                         in mkVec2 x 0.5
                 )

-- create new geometry for heatmap based on polygon and triangles from roofnode
-- and shade points data
createNewGeometry :: Array Vector3 -> Array Face3 -> Array ShadePoint -> Effect BufferGeometry
createNewGeometry polygon faces shadePs = do
    let polyShadePs  = shadePointsForVerts shadePs polygon
        ivs          = indexedVertices polygon
        oldTrs       = getTriangles ivs faces
        newVertices  = indexedVertsForShadePoints shadePs
        newTris      = triangulate oldTrs newVertices
    
        -- merge all vertices into one array
        polyVertNum  = length ivs

        mkVertVec iv = mkVec3 (iv ^. _x) (iv ^. _y) 0.0
        meshVerts    = mkVertVec <$> (ivs <> newVertices)

        -- merge shade intensity values of all vertices
        shades       = polyShadePs <> shadePs

        getIndex iv | iv ^. _vertType == VertPolygon = iv ^. _index
                    | otherwise                      = iv ^. _index + polyVertNum
        -- convert newly generated triangles into Face3 array
        mkFace tri = let v1 = triVertex1 tri
                         v2 = triVertex2 tri
                         v3 = triVertex3 tri
                     in mkFace3 (getIndex v1) (getIndex v2) (getIndex v3)
    newFaces <- traverse mkFace newTris
    
    -- calculate faceVertexUV correctly based on face and uvs
    let uvs = heatmapUVs shades

    geo <- mkBufferGeometry

    -- position
    posAttr <- mkBufferAttribute (vector3Array meshVerts) 3
    setAttribute "position" posAttr geo

    -- faces
    fsAttr <- mkBufferAttribute (face3Array newFaces) 1
    setIndex fsAttr geo

    -- uv
    uvAttr <- mkBufferAttribute (vector2Array uvs) 2
    setAttribute "uv" uvAttr geo

    pure geo
