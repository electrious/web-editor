module Rendering.Racking.BXRendering where

import Prelude

import Data.Function.Memoize (memoize)
import Data.Lens ((^.))
import Data.Meter (meterVal)
import Editor.Common.Lenses (_x, _y, _z)
import Effect.Unsafe (unsafePerformEffect)
import Math (pi)
import Model.Racking.BX.Chassis (Chassis)
import Rendering.Racking.BXChassisObj (bxChassisObjData)
import Rendering.Renderable (class Renderable)
import Renderring.MaterialLoader (blackMaterial)
import Three.Core.Geometry (Geometry)
import Three.Core.Mesh (Mesh, geometry, mkMesh)
import Three.Core.Object3D (setCastShadow, setName, setPosition, setRotation, setScale)
import Three.Loader.ObjLoader (makeOBJLoader2, parseOBJ)
import Three.Math.Euler (mkEuler)
import Three.Math.Vector (mkVec3)
import Unsafe.Coerce (unsafeCoerce)

newtype ChassisRenderable = ChassisRenderable Chassis
instance renderableChassis :: Renderable ChassisRenderable Mesh where
    render (ChassisRenderable c) = do
        m <- mkMesh (chassisGeo "chassis") blackMaterial
        setName "Chassis" m
        setCastShadow false m
        let s = 0.00085
        setScale (mkVec3 s s s) m
        setPosition (mkVec3 (meterVal $ c ^. _x)
                            (meterVal $ c ^. _y)
                            (meterVal $ c ^. _z)) m
        setRotation (mkEuler (pi / 2.0) 0.0 pi) m
        pure m


chassisGeo :: String -> Geometry
chassisGeo = memoize \_ -> unsafePerformEffect do
    loader <- makeOBJLoader2
    let obj = parseOBJ bxChassisObjData loader
    pure $ geometry $ unsafeCoerce obj
