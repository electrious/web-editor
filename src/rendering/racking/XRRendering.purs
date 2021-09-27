module Rendering.Racking.XRRendering where

import Prelude hiding (add)

import Data.Default (def)
import Data.Lens ((.~), (^.))
import Data.Meter (inch, meterVal)
import Data.Traversable (traverse_)
import Editor.Common.Lenses (_clamps, _flashes, _length, _lfeet, _name, _position, _rails, _rotation, _scale, _splices, _stoppers, _x, _y, _z)
import Effect.Unsafe (unsafePerformEffect)
import Math (pi)
import Model.Racking.XR10.Clamp (Clamp)
import Model.Racking.XR10.LFoot (LFoot)
import Model.Racking.XR10.Rail (Rail)
import Model.Racking.XR10.Splice (Splice)
import Model.Racking.XR10.Stopper (Stopper)
import Model.Racking.XR10.XRRackingComponent (XRRackingComponent)
import Taihe.Node (Node, mesh, node)
import Rendering.Racking.Common (buildClamp, renderFlash)
import Renderring.MaterialLoader (blackMaterial)
import Three.Core.Geometry (BoxGeometry, CylinderGeometry, mkBoxGeometry, mkCylinderGeometry)
import Three.Math.Euler (mkEuler)
import Three.Math.Vector (mkVec3)

renderXR :: forall e. XRRackingComponent -> Node e Unit
renderXR x = node (def # _name .~ "XRRackingComponent") do
    traverse_ renderFlash (x ^. _flashes)
    traverse_ renderRail (x ^. _rails)
    traverse_ renderSplice (x ^. _splices)
    traverse_ renderLFoot (x ^. _lfeet)
    traverse_ renderClamp (x ^. _clamps)
    traverse_ renderStopper (x ^. _stoppers)

renderRail :: forall e. Rail -> Node e Unit
renderRail rail = void $ mesh (def # _name .~ "Rail"
                                   # _position .~ pure p
                                   # _scale .~ pure s
                              ) railGeometry blackMaterial
    where s = mkVec3 (meterVal $ rail ^. _length) 1.0 1.0
          p = mkVec3 (meterVal $ rail ^. _x)
                     (meterVal $ rail ^. _y)
                     (meterVal $ rail ^. _z)
        
railGeometry :: BoxGeometry
railGeometry = unsafePerformEffect $ mkBoxGeometry 1.0 h l
    where h = meterVal $ inch 1.0
          l = meterVal $ inch 2.5


renderSplice :: forall e. Splice -> Node e Unit
renderSplice s = void $ mesh (def # _name .~ "Splice"
                                  # _position .~ pure p
                             ) spliceGeometry blackMaterial
    where p = mkVec3 (meterVal $ s ^. _x)
                     (meterVal $ s ^. _y)
                     (meterVal $ s ^. _z)

spliceGeometry :: BoxGeometry
spliceGeometry = unsafePerformEffect $ mkBoxGeometry 0.1778 0.02 0.06


renderLFoot :: forall e. LFoot -> Node e Unit
renderLFoot l = do
    let p = mkVec3 (meterVal $ l ^. _x)
                   (meterVal $ l ^. _y)
                   (meterVal $ l ^. _z)
    node (def # _name .~ "LFoot"
              # _position .~ pure p) do
        void $ mesh (def # _name .~ "bottom"
                         # _position .~ pure (mkVec3 0.0 (-0.019) 0.0)
                    ) lfootBotBox blackMaterial
        void $ mesh (def # _name .~ "side"
                         # _position .~ pure (mkVec3 0.0 0.0 0.035)
                    ) lfootSideBox blackMaterial

lfootBotBox :: BoxGeometry
lfootBotBox = unsafePerformEffect $ mkBoxGeometry 0.047752 0.048768 0.007874

lfootSideBox :: BoxGeometry
lfootSideBox = unsafePerformEffect $ mkBoxGeometry 0.047752 0.010668 0.0762


renderClamp :: forall e. Clamp -> Node e Unit
renderClamp c = buildClamp "Clamp" (mkVec3 (meterVal $ c ^. _x)
                                           (meterVal $ c ^. _y)
                                           (meterVal $ c ^. _z))
                                   (mkEuler 0.0 0.0 0.0)

renderStopper :: forall e. Stopper -> Node e Unit
renderStopper s = void $ mesh (def # _name .~ "Stopper"
                                   # _position .~ pure p
                                   # _rotation .~ pure (mkEuler (pi / 2.0) 0.0 0.0)
                              ) stopperCy blackMaterial
    where p = mkVec3 (meterVal $ s ^. _x)
                     (meterVal $ s ^. _y)
                     (meterVal $ s ^. _z)

stopperCy :: CylinderGeometry
stopperCy = unsafePerformEffect $ mkCylinderGeometry 0.02 0.02 0.036 8 false
