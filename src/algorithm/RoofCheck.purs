module Algorithm.RoofCheck where

import Prelude

import Algorithm.PointInPolygon (pointInPolygon)
import Data.Foldable (any)
import Editor.SceneEvent (SceneMouseMoveEvent)
import Effect (Effect)
import Math.Angle (acos, degreeVal)
import Models.RoofPlate (RoofPlate, getRoofPolygon)
import Three.Core.Face3 (normal)
import Three.Core.Object3D (Object3D, worldToLocal)
import Three.Math.Vector (mkVec2, mkVec3, vecX, vecY, (<.>))

-- | check if there can be a roof at the point under mouse
couldBeRoof :: forall a. Object3D a -> Array RoofPlate -> SceneMouseMoveEvent -> Effect Boolean
couldBeRoof house roofs e = do
    let roofPoly = getRoofPolygon <$> roofs
    
    -- get the local coordinate of the intersection point in the house mesh
    localPoint <- worldToLocal e.point house

    let -- 2D projection of the intersection point
        flatP = mkVec2 (vecX localPoint) (vecY localPoint)
        up = mkVec3 0.0 0.0 1.0
        calcAngle norm = acos $ norm <.> up
    -- check if the point is under any roof
    if any (flip pointInPolygon flatP) roofPoly
        then pure false
        else pure $ degreeVal (calcAngle (normal e.face)) < 60.0
