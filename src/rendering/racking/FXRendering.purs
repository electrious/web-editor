module Rendering.Racking.FXRendering where

import Prelude hiding (add)

import Data.Default (def)
import Data.Foldable (traverse_)
import Data.Lens ((.~), (^.))
import Data.Meter (meterVal)
import Editor.Common.Lenses (_bridges, _flashes, _leftEndCaps, _length, _mounts, _name, _position, _rightEndCaps, _rotation, _scale, _skirts, _x, _y, _z)
import Effect.Unsafe (unsafePerformEffect)
import Math (atan, pi, sqrt)
import Model.Racking.FX.Bridge (Bridge)
import Model.Racking.FX.EndCap (EndCap)
import Model.Racking.FX.FXRackingComponent (FXRackingComponent)
import Model.Racking.FX.Mount (Mount, _clampX, mountRadius)
import Model.Racking.FX.Skirt (Skirt)
import Taihe.Node (Node, mesh, node)
import Rendering.Racking.Common (buildClamp, renderFlash)
import Renderring.MaterialLoader (blackMaterial)
import Three.Core.Geometry (BoxGeometry, CylinderGeometry, mkBoxGeometry, mkCylinderGeometry)
import Three.Core.Mesh (Mesh)
import Three.Math.Euler (Euler, mkEuler)
import Three.Math.Vector (Vector3, mkVec3)



renderFX :: forall e. FXRackingComponent -> Node e Unit
renderFX f = node (def # _name .~ "FXRackingComponent") do
    traverse_ renderFlash (f ^. _flashes)
    traverse_ renderMount (f ^. _mounts)
    traverse_ renderBridge (f ^. _bridges)
    traverse_ renderSkirt (f ^. _skirts)
    traverse_ renderEndCap (f ^. _leftEndCaps <> f ^. _rightEndCaps)

mountBotBox :: BoxGeometry
mountBotBox = unsafePerformEffect $ mkBoxGeometry 0.0508 0.127 0.05

mountEndCy :: CylinderGeometry
mountEndCy = unsafePerformEffect $ mkCylinderGeometry 0.0254 0.0254 0.05 8 false

mountHeadCy :: CylinderGeometry
mountHeadCy = unsafePerformEffect $ mkCylinderGeometry 0.0127 0.0127 0.046 8 false

buildMount :: forall e. String -> Vector3 -> Euler -> Node e Unit
buildMount name pos rot = node (def # _name .~ name
                                    # _position .~ pure pos
                                    # _rotation .~ pure rot) do
    void $ mesh (def # _name .~ "bottom") mountBotBox blackMaterial
    void $ mesh (def # _name .~ "bottom-end-1"
                     # _position .~ pure (mkVec3 0.0 0.06 0.0)
                     # _rotation .~ pure (mkEuler (pi / 2.0) 0.0 0.0)
                ) mountEndCy blackMaterial
    void $ mesh (def # _name .~ "bottom-end-2"
                     # _position .~ pure (mkVec3 0.0 (-0.06) 0.0)
                     # _rotation .~ pure (mkEuler (pi / 2.0) 0.0 0.0)
                ) mountEndCy blackMaterial
    void $ mesh (def # _name .~ "head"
                     # _position .~ pure (mkVec3 0.0 (-0.058) 0.046)
                ) mountHeadCy blackMaterial

renderMount :: forall e. Mount -> Node e Unit
renderMount m = do
    let p = mkVec3 (meterVal $ m ^. _x)
                   (meterVal $ m ^. _y)
                   (meterVal $ m ^. _z)
    node (def # _name .~ "Mount"
              # _position .~ pure p) do
        let bx = meterVal $ m ^. _clampX - m ^. _x

        buildClamp "Clamp-part" (mkVec3 bx (-0.015) 0.0433) (mkEuler 0.0 0.0 0.0)

        -- calculate the center position and rotation of the mount
        -- "a" denotes the end of the mount on flash, (ax is always 0)
        -- "b" denotes the end of the mount with clamp (by is always 0)
        -- "c" denotes center of the mount
        let r = meterVal mountRadius
            aY = sqrt $ r * r - bx * bx
            cX = bx / 2.0
            cY = aY / 2.0

        buildMount "Mount-part" (mkVec3 cX (cY - r / 2.0) 0.0) (mkEuler 0.0 0.0 (atan (-bx / aY)))

renderBridge :: forall e. Bridge -> Node e Unit
renderBridge b = do
    let p = mkVec3 (meterVal $ b ^. _x)
                   (meterVal $ b ^. _y)
                   (meterVal $ b ^. _z)
    node (def # _name .~ "Bridge"
              # _position .~ pure p) do
        void $ mesh (def # _name .~ "top"
                         # _position .~ pure (mkVec3 0.0 0.0 0.02)
                    ) bridgeTopBox blackMaterial
        void $ mesh (def # _name .~ "middle") bridgeMidBox blackMaterial

        void $ mesh (def # _name .~ "bottom"
                         # _position .~ pure (mkVec3 0.0 0.0 (-0.02))
                    ) bridgeBotBox blackMaterial

bridgeTopBox :: BoxGeometry
bridgeTopBox = unsafePerformEffect $ mkBoxGeometry 0.2032 0.05 0.003

bridgeMidBox :: BoxGeometry
bridgeMidBox = unsafePerformEffect $ mkBoxGeometry 0.2032 0.02 0.04

bridgeBotBox :: BoxGeometry
bridgeBotBox = unsafePerformEffect $ mkBoxGeometry 0.2032 0.1 0.003


renderSkirt :: forall e. Skirt -> Node e Mesh
renderSkirt s = mesh (def # _name .~ "Skirt"
                          # _position .~ pure p
                          # _scale .~ pure scale
                     ) skirtBox blackMaterial
    where scale = mkVec3 (meterVal $ s ^. _length) 1.0 1.0
          p = mkVec3 (meterVal $ s ^. _x)
                     (meterVal $ s ^. _y)
                     (meterVal $ s ^. _z)

skirtBox :: BoxGeometry
skirtBox = unsafePerformEffect $ mkBoxGeometry 1.0 0.03 0.04


renderEndCap :: forall e. EndCap -> Node e Mesh
renderEndCap e = mesh (def # _name .~ "EndCap"
                           # _position .~ pure p
                      ) endCapBox blackMaterial
    where p = mkVec3 (meterVal $ e ^. _x)
                     (meterVal $ e ^. _y)
                     (meterVal $ e ^. _z)

endCapBox :: BoxGeometry
endCapBox = unsafePerformEffect $ mkBoxGeometry 0.001 0.03 0.04
