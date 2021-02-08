module HouseBuilder.PolyGeometry where

import Prelude

import Algorithm.Earcut (triangulatePoly)
import Data.Lens ((^.))
import Effect (Effect)
import Model.Polygon (Polygon, _polyVerts)
import Three.Core.Geometry (Geometry, mkGeometry, setElementsNeedUpdate, setFaces, setVertices, setVerticesNeedUpdate)
import Three.Math.Vector (class Vector, getVector)


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
