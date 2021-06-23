module SmartHouse.PolyGeometry where

import Prelude

import Algorithm.Earcut (triangulatePoly)
import Data.Hardware.Size (Size)
import Data.Lens ((^.))
import Data.Meter (meterVal)
import Editor.Common.Lenses (_height, _width)
import Effect (Effect)
import Model.Polygon (Polygon, _polyVerts)
import Three.Core.Geometry (BufferGeometry, mkBufferAttribute, mkBufferGeometry, setAttribute, setIndex)
import Three.Core.TypedArray (vector2Array, vector3Array)
import Three.Math.Vector (class Vector, getVector, mkVec2, vecX, vecY)


-- PolyGeometry is a regular Geometry value
type PolyGeometry = BufferGeometry


mkPolyGeometry :: forall v. Vector v => Polygon v -> v -> Effect PolyGeometry
mkPolyGeometry poly norm = do
    let vs = getVector <$> (poly ^. _polyVerts)
        indices = triangulatePoly poly

        nv = getVector norm
        ns = const nv <$> vs

        mkUV v = mkVec2 0.0 0.0
        uvs = mkUV <$> vs

    geo <- mkBufferGeometry

    posAttr <- mkBufferAttribute (vector3Array vs) 3
    setAttribute "position" posAttr geo

    normAttr <- mkBufferAttribute (vector3Array ns) 3
    setAttribute "normal" normAttr geo

    idxAttr <- mkBufferAttribute indices 1
    setIndex idxAttr geo

    uvAttr <- mkBufferAttribute (vector2Array uvs) 2
    setAttribute "uv" uvAttr geo
    
    pure geo

mkPolyGeometryWithUV :: forall v. Vector v => Size -> Polygon v -> Effect PolyGeometry
mkPolyGeometryWithUV s poly = do
    let vs = getVector <$> (poly ^. _polyVerts)
        indices = triangulatePoly poly

        w = meterVal $ s ^. _width
        h = meterVal $ s ^. _height
        mkUV v = mkVec2 ((vecX v + w / 2.0) / w) ((vecY v + h / 2.0) / h)
        uvs = mkUV <$> vs
    
    geo <- mkBufferGeometry

    posAttr <- mkBufferAttribute (vector3Array vs) 3
    setAttribute "position" posAttr geo

    idxAttr <- mkBufferAttribute indices 1
    setIndex idxAttr geo

    uvAttr <- mkBufferAttribute (vector2Array uvs) 2
    setAttribute "uv" uvAttr geo

    pure geo
