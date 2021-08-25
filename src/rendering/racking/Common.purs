module Rendering.Racking.Common (buildClamp, renderFlash) where

import Prelude hiding (add)

import Data.Default (def)
import Data.Lens ((.~), (^.))
import Data.Meter (inch, meterVal)
import Editor.Common.Lenses (_name, _position, _rotation, _x, _y, _z)
import Effect.Unsafe (unsafePerformEffect)
import Math (pi)
import Model.Racking.Flash (Flash)
import Rendering.Node (Node, mesh, node)
import Renderring.MaterialLoader (blackMaterial)
import Three.Core.Geometry (BoxGeometry, CylinderGeometry, mkBoxGeometry, mkCylinderGeometry)
import Three.Math.Euler (Euler, mkEuler)
import Three.Math.Vector (Vector3, mkVec3)

clampBodyCy :: CylinderGeometry
clampBodyCy = unsafePerformEffect $ mkCylinderGeometry 0.005 0.005 0.06 8 false

clampHeadCy :: CylinderGeometry
clampHeadCy = unsafePerformEffect $ mkCylinderGeometry 0.015 0.015 0.006 8 false

buildClamp :: forall e. String -> Vector3 -> Euler -> Node e Unit
buildClamp name pos rot = node (def # _name .~ name
                                    # _position .~ pure pos
                                    # _rotation .~ pure rot) do
    void $ mesh (def # _name .~ "body"
                     # _position .~ pure (mkVec3 0.0 0.0 0.02)
                     # _rotation .~ pure (mkEuler (pi / 2.0) 0.0 0.0)
                ) clampBodyCy blackMaterial

    void $ mesh (def # _name .~ "head"
                     # _position .~ pure (mkVec3 0.0 0.0 0.043)
                     # _rotation .~ pure (mkEuler (pi / 2.0) 0.0 0.0)
                ) clampHeadCy blackMaterial


renderFlash :: forall e. Flash -> Node e Unit
renderFlash f = void $ mesh (def # _name .~ "Flash"
                                 # _position .~ pure p
                            ) flashGeo blackMaterial
    where p = mkVec3 (meterVal $ f ^. _x)
                     (meterVal $ f ^. _y)
                     (meterVal $ f ^. _z)

flashGeo :: BoxGeometry
flashGeo = unsafePerformEffect $ mkBoxGeometry (meterVal $ inch 9.0)
                                               (meterVal $ inch 12.0)
                                               (meterVal $ inch 0.2)
