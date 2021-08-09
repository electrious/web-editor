module Rendering.Racking.BXRendering where

import Prelude hiding (add)

import Data.Function.Memoize (memoize)
import Data.Lens ((^.))
import Data.Meter (meterVal)
import Data.Traversable (traverse, traverse_)
import Editor.Common.Lenses (_chassis, _x, _y, _z)
import Effect.Class (liftEffect)
import Effect.Unsafe (unsafePerformEffect)
import Math (pi)
import Model.Racking.BX.BXRackingComponent (BXRackingComponent)
import Model.Racking.BX.Chassis (Chassis)
import Rendering.Racking.BXChassisObj (bxChassisObjData)
import Rendering.Renderable (class Renderable, render)
import Renderring.MaterialLoader (blackMaterial)
import Three.Core.Geometry (BufferGeometry)
import Three.Core.Mesh (Mesh, geometry, mkMesh)
import Three.Core.Object3D (Object3D, add, mkObject3D, setCastShadow, setName, setPosition, setRotation, setScale)
import Three.Loader.ObjLoader (makeOBJLoader, parseOBJ)
import Three.Math.Euler (mkEuler)
import Three.Math.Vector (mkVec3)
import Unsafe.Coerce (unsafeCoerce)

newtype BXRackingComponentRenderable = BXRackingComponentRenderable BXRackingComponent
instance renderableBXRackingComponent :: Renderable e BXRackingComponentRenderable Object3D where
    render (BXRackingComponentRenderable b) = do
        comp <- liftEffect mkObject3D
        liftEffect $ setName "BXRackingComponent" comp

        chassis :: Array Mesh <- traverse render (ChassisRenderable <$> b ^. _chassis)
        liftEffect $ traverse_ (flip add comp) chassis

        pure comp

newtype ChassisRenderable = ChassisRenderable Chassis
instance renderableChassis :: Renderable e ChassisRenderable Mesh where
    render (ChassisRenderable c) = liftEffect do
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


chassisGeo :: String -> BufferGeometry
chassisGeo = memoize \_ -> unsafePerformEffect do
    loader <- makeOBJLoader
    let obj = parseOBJ bxChassisObjData loader
    pure $ geometry (unsafeCoerce obj :: Mesh)
