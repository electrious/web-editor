module Editor.PanelNode where

import Prelude hiding (add)

import Custom.Mesh (mkTapDragMesh)
import Data.Enum (class BoundedEnum, class Enum)
import Data.Foldable (traverse_)
import Data.Function.Memoize (class Tabulate, genericTabulate, memoize)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Bounded (genericBottom, genericTop)
import Data.Generic.Rep.Enum (genericCardinality, genericFromEnum, genericPred, genericSucc, genericToEnum)
import Data.Generic.Rep.Show (genericShow)
import Data.Lens (Lens', (^.))
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Meter (meterVal)
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy(..))
import Editor.Common.Lenses (_dragged, _height, _mesh, _orientation, _rackingType, _tapped, _x, _y)
import Editor.Rendering.DefMaterials (loadMaterial)
import Editor.UI.DragInfo (DragInfo, mkDragInfo)
import Effect (Effect)
import Effect.Unsafe (unsafePerformEffect)
import FRP.Event (Event)
import Math (pi)
import Math.Angle (radianVal, sin)
import Model.Hardware.PanelTextureInfo (PanelTextureInfo, _premium, _standard, _standard72)
import Model.Hardware.PanelType (PanelType(..))
import Model.Racking.RackingType (RackingType(..))
import Model.Roof.ArrayConfig (ArrayConfig, _panelLowestZ)
import Model.Roof.Panel (Orientation(..), Panel, panelLong, panelShort, validatedSlope)
import Model.RoofComponent (size)
import Three.Core.Geometry (class IsGeometry, BoxGeometry, mkBoxGeometry)
import Three.Core.Material (class IsMaterial, MeshBasicMaterial, mkMeshBasicMaterialWithTexture, setOpacity)
import Three.Core.Mesh (Mesh, mkMesh)
import Three.Core.Object3D (add, rotateWithEuler, setCastShadow, setName, setPosition, setRenderOrder)
import Three.Loader.TextureLoader (loadTexture, mkTextureLoader)
import Three.Math.Euler (mkEuler)
import Three.Math.Vector (mkVec3)

data PanelOpacity = Opaque
                  | Transparent
derive instance eqPanelOpacity :: Eq PanelOpacity

opacityValue :: PanelOpacity -> Number
opacityValue Opaque      = 1.0
opacityValue Transparent = 0.2

isOpaque :: PanelOpacity -> Boolean
isOpaque Opaque = true
isOpaque _      = false

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
    panel       :: Panel,
    panelObject :: Mesh,
    tapped      :: Event Panel,
    dragged     :: Event (DragInfo Panel),
    materials   :: List MeshBasicMaterial
}

derive instance newtypePanelNode :: Newtype PanelNode _

_panel :: forall t a r. Newtype t { panel :: a | r } => Lens' t a
_panel = _Newtype <<< prop (SProxy :: SProxy "panel")

_panelObject :: forall t a r. Newtype t { panelObject :: a | r } => Lens' t a
_panelObject = _Newtype <<< prop (SProxy :: SProxy "panelObject")

_materials :: forall t a r. Newtype t { materials :: a | r } => Lens' t a
_materials = _Newtype <<< prop (SProxy :: SProxy "materials")

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
mkPanelMesh :: ArrayConfig -> PanelTextureInfo -> PanelType -> Panel -> Effect PanelNode
mkPanelMesh arrCfg info panelType p = do
    -- create the panel body first
    let bodyGeo = panelGeometry unit
        vertGeo = verticalGeometry unit
        horiGeo = horizontalGeometry unit
        -- always use black material for frame around panel
        blackMat = loadMaterial Premium

        rackingType = arrCfg ^. _rackingType
        bodyMat = getPanelMaterial info $ panelTextureType rackingType panelType

    m <- mkTapDragMesh bodyGeo bodyMat
    let mesh = m ^. _mesh
    setName "panel-body" mesh
    let node = PanelNode {
                 panel       : p,
                 panelObject : mesh,
                 tapped      : const p <$> m ^. _tapped,
                 dragged     : mkDragInfo p <$> m ^. _dragged,
                 materials   : (bodyMat : blackMat : Nil)
              }
    -- create frames
    top   <- mkTopFrame horiGeo blackMat
    bot   <- mkBotFrame horiGeo blackMat
    left  <- mkLeftFrame vertGeo blackMat
    right <- mkRightFrame vertGeo blackMat

    -- add frame meshes to panel mesh
    traverse_ (flip add mesh) [top, bot, left, right]
    updateRotation p node
    updatePosition p arrCfg node

    pure node

-- update panel mesh position based on array config and the corresponding panel model
updatePosition :: Panel -> ArrayConfig -> PanelNode -> Effect Unit
updatePosition p arrCfg node = setPosition pv m
    where m  = node ^. _panelObject
          px = meterVal $ p ^. _x
          py = meterVal $ p ^. _y
          z  = meterVal $ arrCfg ^. _panelLowestZ
          pv = case validatedSlope p of
                  Nothing -> mkVec3 px py z
                  Just slope -> let s = size p
                                    h = meterVal (s ^. _height) * 0.5 * sin slope
                                in mkVec3 px py (z + h)

-- update the rotation of the panel mesh
updateRotation :: Panel -> PanelNode -> Effect Unit
updateRotation p node = rotateWithEuler euler m
    where m = node ^. _panelObject
          euler = case validatedSlope p of
                     Nothing -> rotateForNormal $ p ^. _orientation
                     Just slope -> rotateForFlat (p ^. _orientation) slope
          rotateForNormal Landscape = mkEuler 0.0 0.0 (pi / 2.0)
          rotateForNormal Portrait  = mkEuler 0.0 0.0 0.0

          rotateForFlat Landscape slope = mkEuler 0.0 (- radianVal slope) (pi / 2.0)
          rotateForFlat Portrait  slope = mkEuler (radianVal slope) 0.0 0.0

updateOpacity :: PanelOpacity -> PanelNode -> Effect Unit
updateOpacity opacity node = traverse_ (setOpacity (opacityValue opacity)) (node ^. _materials)

enableShadows :: Boolean -> PanelNode -> Effect Unit
enableShadows e node = setCastShadow e (node ^. _panelObject)
