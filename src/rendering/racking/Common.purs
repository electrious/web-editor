module Rendering.Racking.Common (buildClamp) where

import Prelude hiding (add)

import Effect (Effect)
import Effect.Unsafe (unsafePerformEffect)
import Math (pi)
import Renderring.MaterialLoader (blackMaterial)
import Three.Core.Geometry (CylinderGeometry, mkCylinderGeometry)
import Three.Core.Mesh (mkMesh)
import Three.Core.Object3D (Object3D, add, mkObject3D, setName, setPosition, setRotation)
import Three.Math.Euler (mkEuler)
import Three.Math.Vector (mkVec3)

clampBodyCy :: CylinderGeometry
clampBodyCy = unsafePerformEffect $ mkCylinderGeometry 0.005 0.06

clampHeadCy :: CylinderGeometry
clampHeadCy = unsafePerformEffect $ mkCylinderGeometry 0.015 0.006

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
