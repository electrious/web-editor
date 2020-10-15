module Algorithm.Delaunay.Vertex where

import Three.Math.Vector (Vector2, Vector3, vecX, vecY)

class Vertex v where
    vertX :: v -> Number
    vertY :: v -> Number

instance vertexVector2 :: Vertex Vector2 where
    vertX = vecX
    vertY = vecY
instance vertexVector3 :: Vertex Vector3 where
    vertX = vecX
    vertY = vecY
