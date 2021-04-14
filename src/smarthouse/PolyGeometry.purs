module SmartHouse.PolyGeometry where

import Prelude

import Algorithm.Earcut (triangulatePoly)
import Data.Array ((!!))
import Data.Compactable (compact)
import Data.Hardware.Size (Size)
import Data.Lens ((^.))
import Data.Meter (meterVal)
import Editor.Common.Lenses (_height, _width)
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

    geo <- mkGeometry
    setVertices vs geo
    setVerticesNeedUpdate true geo
    setFaces fs geo
    setElementsNeedUpdate true geo

    pure geo

mkPolyGeometryWithUV :: forall v. Vector v => Size -> Polygon v -> Effect PolyGeometry
mkPolyGeometryWithUV s poly = do
    let vs = getVector <$> (poly ^. _polyVerts)
        fs = triangulatePoly poly

        w = meterVal $ s ^. _width
        h = meterVal $ s ^. _height
        mkUV v = mkVec2 ((vecX v + w / 2.0) / w) ((vecY v + h / 2.0) / h)
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
