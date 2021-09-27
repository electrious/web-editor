module Rendering.Racking.BXRendering where

import Prelude hiding (add)

import Data.Default (def)
import Data.Lens ((.~), (^.))
import Data.Meter (meterVal)
import Data.Traversable (traverse_)
import Editor.Common.Lenses (_chassis, _name, _position, _rotation, _scale, _x, _y, _z)
import Effect.Unsafe (unsafePerformEffect)
import Math (pi)
import Model.Racking.BX.BXRackingComponent (BXRackingComponent)
import Model.Racking.BX.Chassis (Chassis)
import Taihe.Node (Node, _castShadow, mesh, node)
import Rendering.Racking.BXChassisObj (bxChassisObjData)
import Renderring.MaterialLoader (blackMaterial)
import Three.Core.Geometry (BufferGeometry)
import Three.Core.Mesh (Mesh, geometry)
import Three.Loader.ObjLoader (makeOBJLoader, parseOBJ)
import Three.Math.Euler (mkEuler)
import Three.Math.Vector (mkVec3)
import Unsafe.Coerce (unsafeCoerce)

renderBX :: forall e. BXRackingComponent -> Node e Unit
renderBX b = node (def # _name .~ "BXRackingComponent") $ traverse_ renderChassis $ b ^. _chassis

renderChassis :: forall e. Chassis -> Node e Mesh
renderChassis c = mesh (def # _name .~ "Chassis"
                            # _castShadow .~ false
                            # _scale .~ pure s
                            # _position .~ pure p
                            # _rotation .~ pure r
                      ) chassisGeo blackMaterial
    where v = 0.00085
          s = mkVec3 v v v
          p = mkVec3 (meterVal $ c ^. _x)
                     (meterVal $ c ^. _y)
                     (meterVal $ c ^. _z)
          r = mkEuler (pi / 2.0) 0.0 pi

chassisGeo :: BufferGeometry
chassisGeo = unsafePerformEffect do
    loader <- makeOBJLoader
    let obj = parseOBJ bxChassisObjData loader
    pure $ geometry (unsafeCoerce obj :: Mesh)
