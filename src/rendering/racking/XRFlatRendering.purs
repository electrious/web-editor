module Rendering.Racking.XRFlatRendering where

import Prelude hiding (add)

import Data.Default (def)
import Data.Foldable (traverse_)
import Data.Lens ((.~), (^.))
import Data.Meter (inch, meterVal)
import Editor.Common.Lenses (_baseMounts, _clamps, _height, _length, _name, _position, _rails, _rotation, _scale, _splices, _stoppers, _supportRails, _tiltLegs, _x, _y, _z)
import Effect.Unsafe (unsafePerformEffect)
import Math (pi)
import Math.Angle (Angle, radianVal)
import Model.Racking.XR10.Clamp (Clamp)
import Model.Racking.XR10.Rail (Rail)
import Model.Racking.XR10.Splice (Splice)
import Model.Racking.XR10.Stopper (Stopper)
import Model.Racking.XRFlat.QBaseMount (QBaseMount)
import Model.Racking.XRFlat.SupportRail (SupportRail)
import Model.Racking.XRFlat.TiltLeg (TiltLeg)
import Model.Racking.XRFlat.XRFlatRackingComponent (XRFlatRackingComponent)
import Taihe.Node (Node, mesh, node)
import Rendering.Racking.Common (buildClamp)
import Rendering.Racking.XRRendering (railGeometry, spliceGeometry, stopperCy)
import Renderring.MaterialLoader (blackMaterial)
import Three.Core.Geometry (BoxGeometry, CylinderGeometry, mkBoxGeometry, mkCylinderGeometry)
import Three.Math.Euler (mkEuler)
import Three.Math.Vector (mkVec3)


renderXRFlat :: forall e. Angle -> XRFlatRackingComponent -> Node e Unit
renderXRFlat slope x = node (def # _name .~ "XRFlatRackingComponent") do
        traverse_ (renderRail slope) (x ^. _rails)
        traverse_ (renderSplice slope) (x ^. _splices)
        traverse_ (renderClamp slope) (x ^. _clamps)
        traverse_ (renderStopper slope) (x ^. _stoppers)
        traverse_ renderSupportRail (x ^. _supportRails)
        traverse_ renderQBaseMount (x ^. _baseMounts)
        traverse_ renderTiltLeg (x ^. _tiltLegs)

renderRail :: forall e. Angle -> Rail -> Node e Unit
renderRail slope r = void $ mesh (def # _name .~ "Rail"
                                      # _position .~ pure p
                                      # _rotation .~ pure rot
                                      # _scale .~ pure s) railGeometry blackMaterial
    where s = mkVec3 (meterVal $ r ^. _length) 1.0 1.0
          p = mkVec3 (meterVal $ r ^. _x)
                     (meterVal $ r ^. _y)
                     (meterVal $ r ^. _z)
          rot = mkEuler (radianVal slope) 0.0 0.0

renderSplice :: forall e. Angle -> Splice -> Node e Unit
renderSplice slope s = void $ mesh (def # _name .~ "Splice"
                                        # _position .~ pure p
                                        # _rotation .~ pure r) spliceGeometry blackMaterial
    where p = mkVec3 (meterVal $ s ^. _x)
                     (meterVal $ s ^. _y)
                     (meterVal $ s ^. _z)
          r = mkEuler (radianVal slope) 0.0 0.0

renderClamp :: forall e. Angle -> Clamp -> Node e Unit
renderClamp slope c = buildClamp "Clamp" p r
    where p = mkVec3 (meterVal $ c ^. _x)
                     (meterVal $ c ^. _y)
                     (meterVal $ c ^. _z)
          r = mkEuler (radianVal slope) 0.0 0.0

renderStopper :: forall e. Angle -> Stopper -> Node e Unit
renderStopper slope s = void $ mesh (def # _name .~ "Stopper"
                                         # _position .~ pure p
                                         # _rotation .~ pure r
                                    ) stopperCy blackMaterial
    where r = mkEuler (pi / 2.0 + radianVal slope) 0.0 0.0
          p = mkVec3 (meterVal $ s ^. _x)
                     (meterVal $ s ^. _y)
                     (meterVal $ s ^. _z)

renderSupportRail :: forall e. SupportRail -> Node e Unit
renderSupportRail s = void $ mesh (def # _name .~ "SupportRail"
                                       # _position .~ pure p
                                       # _scale .~ pure scale
                                  ) supportRailGeo blackMaterial
    where scale = mkVec3 1.0 (meterVal $ s ^. _length) 1.0
          p = mkVec3 (meterVal $ s ^. _x)
                     (meterVal $ s ^. _y)
                     (meterVal $ s ^. _z)

supportRailGeo :: BoxGeometry
supportRailGeo = unsafePerformEffect $ mkBoxGeometry l 1.0 l
    where l = meterVal $ inch 1.0


renderQBaseMount :: forall e. QBaseMount -> Node e Unit
renderQBaseMount q = do
    let p = mkVec3 (meterVal $ q ^. _x)
                   (meterVal $ q ^. _y)
                   (meterVal $ q ^. _z)
        r = mkEuler (pi / 2.0) 0.0 0.0
    node (def # _name .~ "QBaseMount"
              # _position .~ pure p
              # _rotation .~ pure r) do
        void $ mesh (def # _name .~ "base"
                         # _position .~ pure (mkVec3 0.0 (meterVal $ inch 0.25) 0.0)
                    ) qbMountBaseCy blackMaterial
        void $ mesh (def # _name .~ "stick"
                         # _position .~ pure (mkVec3 0.0 (meterVal (q ^. _height) / 2.0) 0.0)
                         # _scale .~ pure (mkVec3 1.0 (meterVal $ q ^. _height) 1.0)
                    ) qbMountStickCy blackMaterial

qbMountBaseCy :: CylinderGeometry
qbMountBaseCy = unsafePerformEffect $ mkCylinderGeometry r r (meterVal $ inch 0.5) 8 false
    where r = meterVal $ inch 3.0

qbMountStickCy :: CylinderGeometry
qbMountStickCy = unsafePerformEffect $ mkCylinderGeometry r r 1.0 8 false
    where r = meterVal $ inch 0.8


renderTiltLeg :: forall e. TiltLeg -> Node e Unit
renderTiltLeg t = void $ mesh (def # _name .~ "TiltLeg"
                                   # _position .~ pure p
                                   # _scale .~ pure s
                              ) tiltLegBox blackMaterial
    where p = mkVec3 (meterVal $ t ^. _x)
                     (meterVal $ t ^. _y)
                     (meterVal $ t ^. _z)
          s = mkVec3 1.0 1.0 (meterVal $ t ^. _length)

tiltLegBox :: BoxGeometry
tiltLegBox = unsafePerformEffect $ mkBoxGeometry l l 1.0
    where l = meterVal $ inch 0.8
