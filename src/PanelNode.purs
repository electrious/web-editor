module Editor.PanelNode where

import Prelude

import Data.Default (class Default)
import Data.Enum (class BoundedEnum, class Enum, cardinality, fromEnum, toEnum)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Bounded (genericBottom, genericTop)
import Data.Generic.Rep.Enum (genericCardinality, genericFromEnum, genericPred, genericSucc, genericToEnum)
import Data.Generic.Rep.Show (genericShow)
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
import Model.Hardware.PanelType (PanelType(..))
import Model.Racking.RackingType (RackingType(..))
import Model.Roof.ArrayConfig (ArrayConfig, _panelLowestZ)
import Model.Roof.Panel (Orientation(..), Panel, _orientation, _panelX, _panelY, panelLong, panelShort, panelSize, validatedSlope)
import Three.Core.Geometry (mkBoxGeometry)
import Three.Core.Material (MeshBasicMaterial, mkMeshBasicMaterialWithTexture)
import Three.Core.Mesh (Mesh, mkMesh)
import Three.Core.Object3D (Object3D, mkObject3D, rotateWithEuler, setName, setPosition)
import Three.Loader.TextureLoader (loadTexture, mkTextureLoader)
import Three.Math.Euler (mkEuler)
import Three.Math.Vector (mkVec3)

-- texture info needed to render panels
newtype PanelTextureInfo = PanelTextureInfo {
    standard   :: Maybe String,
    premium    :: Maybe String,
    standard72 :: Maybe String
}

derive instance newtypePanelTextureInfo :: Newtype PanelTextureInfo _
instance defaultPanelTextureInfo :: Default PanelTextureInfo where
    def = PanelTextureInfo { standard   : Nothing,
                             premium    : Nothing,
                             standard72 : Nothing
                           }

_standard :: Lens' PanelTextureInfo (Maybe String)
_standard = _Newtype <<< prop (SProxy :: SProxy "standard")

_premium :: Lens' PanelTextureInfo (Maybe String)
_premium = _Newtype <<< prop (SProxy :: SProxy "premium")

_standard72 :: Lens' PanelTextureInfo (Maybe String)
_standard72 = _Newtype <<< prop (SProxy :: SProxy "standard72")


-- texture type used for panels
data PanelTextureType = PremiumTexture
                      | StandardTexture
                      | Standard72Texture

derive instance genericPanelTextureType :: Generic PanelTextureType _
derive instance eqPanelTextureType :: Eq PanelTextureType
derive instance ordPanelTextureType :: Ord PanelTextureType
instance showPanelTextureType :: Show PanelTextureType where
    show = genericShow
instance boundPanelTextureType :: Bounded PanelTextureType where
    top = genericTop
    bottom = genericBottom
instance enumPanelTextureType :: Enum PanelTextureType where
    succ = genericSucc
    pred = genericPred
instance boundEnumPanelTextureType :: BoundedEnum PanelTextureType where
    cardinality = genericCardinality
    toEnum = genericToEnum
    fromEnum = genericFromEnum

-- get panel texture type based on the racking system and panel type
panelType :: RackingType -> PanelType -> PanelTextureType
panelType BX _       = Standard72Texture
panelType _ Premium  = PremiumTexture
panelType _ Standard = StandardTexture

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
                     Just slope -> rotateForFlat (p ^. _orientation) slope
          rotateForNormal Landscape = mkEuler 0.0 0.0 (pi / 2.0)
          rotateForNormal Portrait  = mkEuler 0.0 0.0 0.0

          rotateForFlat Landscape slope = mkEuler 0.0 (- radianVal slope) (pi / 2.0)
          rotateForFlat Portrait  slope = mkEuler (radianVal slope) 0.0 0.0
