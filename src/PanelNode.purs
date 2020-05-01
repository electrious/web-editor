module Editor.PanelNode where

import Prelude

import Data.Hardware.Size (_height)
import Data.Lens (Lens', (^.))
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..))
import Data.Meter (meterVal)
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy(..))
import Effect (Effect)
import Math (pi)
import Math.Angle (radianVal, sin)
import Model.Roof.ArrayConfig (ArrayConfig(..), _panelLowestZ)
import Model.Roof.Panel (Orientation(..), Panel(..), _orientation, _panelX, _panelY, panelLong, panelShort, panelSize, validatedSlope)
import Model.Roof.Panel as P
import Three.Core.Geometry (mkBoxGeometry)
import Three.Core.Material (MeshBasicMaterial, mkMeshBasicMaterialWithTexture)
import Three.Core.Mesh (Mesh, mkMesh)
import Three.Core.Object3D (Object3D, mkObject3D, rotateWithEuler, setName, setPosition)
import Three.Loader.TextureLoader (loadTexture, mkTextureLoader)
import Three.Math.Euler (mkEuler)
import Three.Math.Vector (mkVec3)

newtype PanelNode a = PanelNode {
    panelId     :: Int,
    panelObject :: Object3D a
}

derive instance newtypePanelNode :: Newtype (PanelNode a) _

_panelId :: forall a. Lens' (PanelNode a) Int
_panelId = _Newtype <<< prop (SProxy :: SProxy "panelId")

_panelObject :: forall a. Lens' (PanelNode a) (Object3D a)
_panelObject = _Newtype <<< prop (SProxy :: SProxy "panelObject")

-- create material for panel node with the provided image url
mkPanelMaterial :: forall a. String -> Effect (MeshBasicMaterial a)
mkPanelMaterial imagePath = mkTextureLoader >>= loadTexture imagePath >>= mkMeshBasicMaterialWithTexture

-- | make a default panel mesh node
mkPanelMesh :: forall a. String -> Effect (Mesh a)
mkPanelMesh imagePath = do
    mat <- mkPanelMaterial imagePath
    geo <- mkBoxGeometry (meterVal panelLong) (meterVal panelShort) 0.05

    node <- mkMesh geo mat
    setName "panel" node

    pure node

-- update panel mesh position based on array config and the corresponding panel model
updatePosition :: forall a. ArrayConfig -> Panel -> Mesh a -> Effect Unit
updatePosition arrCfg p m = setPosition pv m
    where px = meterVal $ p ^. _panelX
          py = meterVal $ p ^. _panelY
          z = meterVal $ arrCfg ^. _panelLowestZ
          pv = case validatedSlope p of
                  Nothing -> mkVec3 px py z
                  Just slope -> let s = panelSize p
                                    h = meterVal (s ^. _height) * 0.5 * sin slope
                                in mkVec3 px py (z + h)

-- update the rotation of the panel mesh
updateRotation :: forall a. Panel -> Mesh a -> Effect Unit
updateRotation p m = rotateWithEuler euler m
    where euler = case validatedSlope p of
                     Nothing -> rotateForNormal $ p ^. _orientation
                     Just slope -> rotateForFlat (p ^> _orientation) slope
          rotateForNormal Landscape = mkEuler 0.0 0.0 (pi / 2)
          rotateForNormal Portrait  = mkEuler 0.0 0.0 0.0

          rotateForFlat Landscape slope = mkEuler 0.0 (- radianVal slope) (pi / 2)
          rotateForFlat Portrait  slope = mkEuler (radianVal slope) 0.0 0.0
