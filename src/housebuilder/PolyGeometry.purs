module HouseBuilder.PolyGeometry where

import Prelude

import Algorithm.Earcut (triangulatePoly)
import Data.Array ((!!))
import Data.Compactable (compact)
import Data.Lens ((^.))
import Effect (Effect)
import Model.Polygon (Polygon, _polyVerts)
import Three.Core.Face3 (indexA, indexB, indexC)
import Three.Core.Geometry (Geometry, mkGeometry, setElementsNeedUpdate, setFaces, setUVs, setUVsNeedUpdate, setVertices, setVerticesNeedUpdate)
import Three.Math.Vector (class Vector, getVector, mkVec2, vecX, vecY)


-- PolyGeometry is a regular Geometry value
type PolyGeometry = Geometry


mkPolyGeometry :: forall v. Vector v => Polygon v -> Effect PolyGeometry
mkPolyGeometry poly = do
    let vs = getVector <$> (poly ^. _polyVerts)
        fs = triangulatePoly poly

        mkUV v = mkVec2 ((vecX v + 50.0) / 100.0) ((vecY v + 23.0) / 46.5)
        uvs = mkUV <$> vs
        mkFaceUVs f = compact [uvs !! indexA f, uvs !! indexB f, uvs !! indexC f]
        faceUVs = mkFaceUVs <$> fs
        
    geo <- mkGeometry
    setVertices vs geo
    setVerticesNeedUpdate true geo
    setFaces fs geo
    setElementsNeedUpdate true geo

    setUVs faceUVs geo
    setUVsNeedUpdate true geo
    
    pure geo
