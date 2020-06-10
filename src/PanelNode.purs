module Editor.PanelNode where

import Prelude hiding (add)

import Data.Enum (class BoundedEnum, class Enum)
import Data.Foldable (traverse_)
import Data.Function.Memoize (class Tabulate, genericTabulate, memoize)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Bounded (genericBottom, genericTop)
import Data.Generic.Rep.Enum (genericCardinality, genericFromEnum, genericPred, genericSucc, genericToEnum)
import Data.Generic.Rep.Show (genericShow)
import Data.Lens (Lens', view, (^.))
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Meter (meterVal)
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy(..))
import Editor.ArrayBuilder (ArrayBuilder, getArrayConfig, getPanelType, getTextureInfo)
import Editor.Common.Lenses (_height, _orientation, _rackingType, _x, _y)
import Editor.Rendering.DefMaterials (loadMaterial)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Unsafe (unsafePerformEffect)
import FRP.Dynamic (Dynamic, performDynamic)
import Math (pi)
import Math.Angle (radianVal, sin)
import Model.Hardware.PanelTextureInfo (PanelTextureInfo, _premium, _standard, _standard72)
import Model.Hardware.PanelType (PanelType(..))
import Model.Racking.RackingType (RackingType(..))
import Model.Roof.ArrayConfig (ArrayConfig, _panelLowestZ)
import Model.Roof.Panel (Orientation(..), Panel, panelLong, panelShort, panelSize, validatedSlope)
import Three.Core.Geometry (class IsGeometry, BoxGeometry, mkBoxGeometry)
import Three.Core.Material (class IsMaterial, MeshBasicMaterial, mkMeshBasicMaterialWithTexture)
import Three.Core.Mesh (Mesh, mkMesh)
import Three.Core.Object3D (class IsObject3D, Object3D, add, rotateWithEuler, setName, setPosition, setRenderOrder)
import Three.Loader.TextureLoader (loadTexture, mkTextureLoader)
import Three.Math.Euler (mkEuler)
import Three.Math.Vector (mkVec3)

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
instance tabulatePanelTextureType :: Tabulate PanelTextureType where
    tabulate = genericTabulate

-- get panel texture type based on the racking system and panel type
panelTextureType :: RackingType -> PanelType -> PanelTextureType
panelTextureType BX _       = Standard72Texture
panelTextureType _ Premium  = PremiumTexture
panelTextureType _ Standard = StandardTexture

-- create material for panel node with the provided image url
mkPanelMaterial :: String -> Effect MeshBasicMaterial
mkPanelMaterial imagePath = mkTextureLoader >>= loadTexture imagePath >>= mkMeshBasicMaterialWithTexture

-- | memoized function to get panel material for the corresponding panel texture type
getPanelMaterial :: PanelTextureInfo -> PanelTextureType -> MeshBasicMaterial
getPanelMaterial info = memoize (unsafePerformEffect <<< mkPanelMaterial <<< imageUrl)
    where imageUrl PremiumTexture    = fromMaybe "" $ info ^. _premium
          imageUrl StandardTexture   = fromMaybe "" $ info ^. _standard
          imageUrl Standard72Texture = fromMaybe "" $ info ^. _standard72

-- | memoized function to create panel body's geometry
panelGeometry :: Unit -> BoxGeometry
panelGeometry = memoize (const $ unsafePerformEffect $ mkBoxGeometry (meterVal panelShort) (meterVal panelLong) 0.04)

-- | memoized function to create panel vertical frame
verticalGeometry :: Unit -> BoxGeometry
verticalGeometry = memoize (const $ unsafePerformEffect $ mkBoxGeometry 0.01 (meterVal panelLong) 0.05)

-- | memoized function to create panel horizontal frame
horizontalGeometry :: Unit -> BoxGeometry
horizontalGeometry = memoize (const $ unsafePerformEffect $ mkBoxGeometry (meterVal panelShort) 0.01 0.05)

newtype PanelNode = PanelNode {
    panelId     :: Int,
    panelObject :: Object3D
}

derive instance newtypePanelNode :: Newtype PanelNode _

_panelId :: Lens' PanelNode Int
_panelId = _Newtype <<< prop (SProxy :: SProxy "panelId")

_panelObject :: Lens' PanelNode Object3D
_panelObject = _Newtype <<< prop (SProxy :: SProxy "panelObject")

mkTopFrame :: forall geo mat. IsGeometry geo => IsMaterial mat => geo -> mat -> Effect Mesh
mkTopFrame geo mat = do
    top <- mkMesh geo mat
    setName "top" top
    setPosition (mkVec3 0.0 (meterVal panelLong / 2.0) 0.0) top
    setRenderOrder 9 top
    pure top

mkBotFrame :: forall geo mat. IsGeometry geo => IsMaterial mat => geo -> mat -> Effect Mesh
mkBotFrame geo mat = do
    bot <- mkMesh geo mat
    setName "bottom" bot
    setPosition (mkVec3 0.0 (- meterVal panelLong / 2.0) 0.0) bot
    setRenderOrder 9 bot
    pure bot

mkLeftFrame :: forall geo mat. IsGeometry geo => IsMaterial mat => geo -> mat -> Effect Mesh
mkLeftFrame geo mat = do
    left <- mkMesh geo mat
    setName "left" left
    setPosition (mkVec3 (- meterVal panelShort / 2.0) 0.0 0.0) left
    setRenderOrder 9 left
    pure left

mkRightFrame :: forall geo mat. IsGeometry geo => IsMaterial mat => geo -> mat -> Effect Mesh
mkRightFrame geo mat = do
    right <- mkMesh geo mat
    setName "right" right
    setPosition (mkVec3 (meterVal panelShort / 2.0) 0.0 0.0) right
    setRenderOrder 9 right
    pure right

-- | make a default panel mesh node
mkPanelMesh :: Panel -> ArrayBuilder (Dynamic Mesh)
mkPanelMesh p = do
    arrCfgDyn    <- getArrayConfig
    info         <- getTextureInfo
    panelTypeDyn <- getPanelType

    -- create the panel body first
    let bodyGeo = panelGeometry unit
        vertGeo = verticalGeometry unit
        horiGeo = horizontalGeometry unit
        -- always use black material for frame around panel
        blackMat = loadMaterial Premium

        rackingTypeDyn = view _rackingType <$> arrCfgDyn
        bodyMatDyn = getPanelMaterial info <$> (panelTextureType <$> rackingTypeDyn <*> panelTypeDyn)

        mkBody geo mat = do
            n <- mkMesh geo mat
            setName "panel-body" n
            pure n
        nodeDyn = performDynamic $ mkBody bodyGeo <$> bodyMatDyn    
    -- create frames
    top   <- liftEffect $ mkTopFrame horiGeo blackMat
    bot   <- liftEffect $ mkBotFrame horiGeo blackMat
    left  <- liftEffect $ mkLeftFrame vertGeo blackMat
    right <- liftEffect $ mkRightFrame vertGeo blackMat

    -- add frame meshes to panel mesh
    let addFrame n = traverse_ (flip add n) [top, bot, left, right] *> pure n
        
        newNodeDyn = performDynamic $ (addFrame >=> updateRotation p) <$> nodeDyn
    pure $ performDynamic $ updatePosition p <$> arrCfgDyn <*> newNodeDyn

-- update panel mesh position based on array config and the corresponding panel model
updatePosition :: forall a. IsObject3D a => Panel -> ArrayConfig -> a -> Effect a
updatePosition p arrCfg m = setPosition pv m *> pure m
    where px = meterVal $ p ^. _x
          py = meterVal $ p ^. _y
          z = meterVal $ arrCfg ^. _panelLowestZ
          pv = case validatedSlope p of
                  Nothing -> mkVec3 px py z
                  Just slope -> let s = panelSize p
                                    h = meterVal (s ^. _height) * 0.5 * sin slope
                                in mkVec3 px py (z + h)

-- update the rotation of the panel mesh
updateRotation :: forall a. IsObject3D a => Panel -> a -> Effect a
updateRotation p m = rotateWithEuler euler m *> pure m
    where euler = case validatedSlope p of
                     Nothing -> rotateForNormal $ p ^. _orientation
                     Just slope -> rotateForFlat (p ^. _orientation) slope
          rotateForNormal Landscape = mkEuler 0.0 0.0 (pi / 2.0)
          rotateForNormal Portrait  = mkEuler 0.0 0.0 0.0

          rotateForFlat Landscape slope = mkEuler 0.0 (- radianVal slope) (pi / 2.0)
          rotateForFlat Portrait  slope = mkEuler (radianVal slope) 0.0 0.0
