module Model.HouseEditor.HousePoint where


import Prelude

import Data.Default (def)
import Data.Lens (Lens', (.~), (^.))
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Newtype (class Newtype)
import Editor.Common.Lenses (_name, _position)
import Effect.Unsafe (unsafePerformEffect)
import Rendering.Node (mesh)
import Rendering.NodeRenderable (class NodeRenderable)
import Three.Core.Geometry (CircleGeometry, mkCircleGeometry)
import Three.Core.Material (MeshBasicMaterial, mkMeshBasicMaterial)
import Three.Core.Mesh (Mesh)
import Three.Math.Vector (Vector3)


newtype RidgePoint = RidgePoint Vector3

derive instance newtypeRidgePoint :: Newtype RidgePoint _

ridgePoint :: Vector3 -> RidgePoint
ridgePoint = RidgePoint

_ridgePointPos :: Lens' RidgePoint Vector3
_ridgePointPos = _Newtype


newtype GutterPoint = GutterPoint Vector3

derive instance newtypeGutterPoint :: Newtype GutterPoint _

gutterPoint :: Vector3 -> GutterPoint
gutterPoint = GutterPoint

_gutterPointPos :: Lens' GutterPoint Vector3
_gutterPointPos = _Newtype


gutterGeo :: CircleGeometry
gutterGeo = unsafePerformEffect $ mkCircleGeometry 0.2 16

gutterMat :: MeshBasicMaterial
gutterMat = unsafePerformEffect $ mkMeshBasicMaterial 0x00ee00

instance nodeRenderableGutterPoint :: NodeRenderable e GutterPoint Mesh where
    render p = mesh prop gutterGeo gutterMat
        where prop = def # _name     .~ "gutter-point"
                         # _position .~ pure (p ^. _gutterPointPos)
