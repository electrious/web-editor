module Rendering.Racking.Rafter where

import Prelude

import Data.Default (def)
import Data.Lens ((.~), (^.))
import Data.Meter (inch, meterVal)
import Data.Traversable (class Traversable, traverse_)
import Editor.Common.Lenses (_length, _name, _position, _scale)
import Effect.Unsafe (unsafePerformEffect)
import Model.Racking.Rafter (Rafter)
import Model.RoofComponent (compX, compY)
import Rendering.Node (Node, mesh)
import Three.Core.Geometry (BoxGeometry, mkBoxGeometry)
import Three.Core.Material (MeshBasicMaterial, mkMeshBasicMaterial)
import Three.Math.Vector (mkVec3)

renderRafters :: forall e f. Traversable f => f Rafter -> Node e Unit
renderRafters = traverse_ renderRafter

renderRafter :: forall e. Rafter -> Node e Unit
renderRafter rafter = void $ mesh (def # _name .~ "Rafter"
                                       # _position .~ pure p
                                       # _scale .~ pure s) rafterGeo rafterMat
    where p = mkVec3 (meterVal $ compX rafter)
                     (meterVal $ compY rafter)
                     v
          s = mkVec3 v
                     (meterVal $ rafter ^. _length)
                     v
          v = meterVal $ inch 0.1


rafterGeo :: BoxGeometry
rafterGeo = unsafePerformEffect $ mkBoxGeometry 1.0 1.0 1.0

rafterMat :: MeshBasicMaterial
rafterMat = unsafePerformEffect $ mkMeshBasicMaterial 0xdca757
