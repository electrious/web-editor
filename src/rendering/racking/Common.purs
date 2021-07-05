module Rendering.Racking.Common (buildClamp, FlashRenderable(..)) where

import Prelude hiding (add)

import Data.Lens ((^.))
import Data.Meter (inch, meterVal)
import Editor.Common.Lenses (_x, _y, _z)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Unsafe (unsafePerformEffect)
import Math (pi)
import Model.Racking.Flash (Flash)
import Rendering.Renderable (class Renderable)
import Renderring.MaterialLoader (blackMaterial)
import Three.Core.Geometry (BoxGeometry, CylinderGeometry, mkBoxGeometry, mkCylinderGeometry)
import Three.Core.Mesh (Mesh, mkMesh)
import Three.Core.Object3D (Object3D, add, mkObject3D, setName, setPosition, setRotation)
import Three.Math.Euler (mkEuler)
import Three.Math.Vector (mkVec3)

clampBodyCy :: CylinderGeometry
clampBodyCy = unsafePerformEffect $ mkCylinderGeometry 0.005 0.005 0.06 8 false

clampHeadCy :: CylinderGeometry
clampHeadCy = unsafePerformEffect $ mkCylinderGeometry 0.015 0.015 0.006 8 false

buildClamp :: Effect Object3D
buildClamp = do
    body <- mkMesh clampBodyCy blackMaterial
    setName "body" body
    setRotation (mkEuler (pi / 2.0) 0.0 0.0) body
    setPosition (mkVec3 0.0 0.0 0.02) body

    head <- mkMesh clampHeadCy blackMaterial
    setName "head" head
    setRotation (mkEuler (pi / 2.0) 0.0 0.0) head
    setPosition (mkVec3 0.0 0.0 0.043) head

    m <- mkObject3D
    setName "Clamp" m
    add body m
    add head m

    pure m

newtype FlashRenderable = FlashRenderable Flash
instance renderableFlash :: Renderable e FlashRenderable Mesh where
    render (FlashRenderable f) = liftEffect do
        m <- mkMesh flashGeo blackMaterial
        setName "Flash" m
        setPosition (mkVec3 (meterVal $ f ^. _x)
                            (meterVal $ f ^. _y)
                            (meterVal $ f ^. _z)) m
        pure m

flashGeo :: BoxGeometry
flashGeo = unsafePerformEffect $ mkBoxGeometry (meterVal $ inch 9.0)
                                               (meterVal $ inch 12.0)
                                               (meterVal $ inch 0.2)
