module Editor.PanelNode where

import Prelude hiding (add)

import Taihe.Mesh (TapDragMesh, mkTapDragMesh)
import Data.Enum (class BoundedEnum, class Enum)
import Data.Foldable (traverse_)
import Data.Function.Memoize (class Tabulate, genericTabulate, memoize, memoize2)
import Data.Generic.Rep (class Generic)
import Data.Bounded.Generic (genericBottom, genericTop)
import Data.Enum.Generic (genericCardinality, genericFromEnum, genericPred, genericSucc, genericToEnum)
import Data.Show.Generic (genericShow)
import Data.Lens (Lens', view, (%~), (.~), (^.))
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Meter (meterVal)
import Data.Newtype (class Newtype)
import Type.Proxy (Proxy(..))
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
import Model.Roof.Panel (Orientation(..), Panel, addDelta, panelLong, panelShort, validatedSlope)
import Model.RoofComponent (size)
import Three.Core.Geometry (class IsGeometry, BoxGeometry, mkBoxGeometry)
import Three.Core.Material (class IsMaterial, MeshBasicMaterial, mkMeshBasicMaterialWithTexture, setOpacity, setTransparent)
import Three.Core.Mesh (Mesh, mkMesh, setMaterial)
import Three.Core.Object3D (class IsObject3D, add, position, rotateWithEuler, setCastShadow, setName, setPosition, setRenderOrder, toObject3D)
import Three.Loader.TextureLoader (loadTexture, mkTextureLoader)
import Three.Math.Euler (mkEuler)
import Three.Math.Vector (Vector3, mkVec3)
import Three.Math.Vector as Vector

data PanelOpacity = Opaque
                  | Transparent
derive instance eqPanelPanelOpacity :: Eq PanelOpacity
derive instance genericPanelOpacity :: Generic PanelOpacity _
instance showPanelOpacity :: Show PanelOpacity where
    show = genericShow

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
getPanelMaterial :: Boolean -> String -> MeshBasicMaterial
getPanelMaterial = memoize2 (\_ imagePath -> unsafePerformEffect $ mkPanelMaterial imagePath)

materialUrl :: PanelTextureInfo -> PanelTextureType -> String
materialUrl info tt = fromMaybe "" $ imageUrl tt
    where imageUrl PremiumTexture    = info ^. _premium
          imageUrl StandardTexture   = info ^. _standard
          imageUrl Standard72Texture = info ^. _standard72

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
    panelObject :: TapDragMesh,
    tapped      :: Event Panel,
    dragged     :: Event (DragInfo Panel),
    materials   :: List MeshBasicMaterial,
    materialURL :: String
}

derive instance newtypePanelNode :: Newtype PanelNode _
instance isObject3DPanelNode :: IsObject3D PanelNode where
    toObject3D = toObject3D <<< view _panelObject

_panel :: forall t a r. Newtype t { panel :: a | r } => Lens' t a
_panel = _Newtype <<< prop (Proxy :: Proxy "panel")

_panelObject :: forall t a r. Newtype t { panelObject :: a | r } => Lens' t a
_panelObject = _Newtype <<< prop (Proxy :: Proxy "panelObject")

_materials :: forall t a r. Newtype t { materials :: a | r } => Lens' t a
_materials = _Newtype <<< prop (Proxy :: Proxy "materials")

_materialURL :: forall t a r. Newtype t { materialURL :: a | r } => Lens' t a
_materialURL = _Newtype <<< prop (Proxy :: Proxy "materialURL")

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
mkPanelNode :: ArrayConfig -> PanelTextureInfo -> PanelType -> Panel -> Boolean -> Effect PanelNode
mkPanelNode arrCfg info panelType p isTemp = do
    -- create the panel body first
    let bodyGeo = panelGeometry unit
        vertGeo = verticalGeometry unit
        horiGeo = horizontalGeometry unit
        -- always use black material for frame around panel
        blackMat = loadMaterial Premium

        rackingType = arrCfg ^. _rackingType
        matUrl      = materialUrl info $ panelTextureType rackingType panelType
        -- use different material for normal and temp panels
        bodyMat     = getPanelMaterial isTemp matUrl

    m <- mkTapDragMesh bodyGeo bodyMat
    setName "panel" m
    let node = PanelNode {
                 panel       : p,
                 panelObject : m,
                 tapped      : const p <$> m ^. _tapped,
                 dragged     : mkDragInfo p <$> m ^. _dragged,
                 materials   : (bodyMat : blackMat : Nil),
                 materialURL : matUrl
              }
    -- create frames
    top   <- mkTopFrame horiGeo blackMat
    bot   <- mkBotFrame horiGeo blackMat
    left  <- mkLeftFrame vertGeo blackMat
    right <- mkRightFrame vertGeo blackMat

    -- add frame meshes to panel mesh
    traverse_ (flip add m) [top, bot, left, right]
    updateRotation p node
    updatePosition p arrCfg node

    when isTemp $ updateOpacityVal 0.7 node

    pure node

changePanel :: ArrayConfig -> Panel -> PanelNode -> Effect PanelNode
changePanel arrCfg p node = do
    updateRotation p node
    updatePosition p arrCfg node
    pure $ panelUpdated $ node # _panel .~ p


moveBy :: Vector3 -> PanelNode -> Effect PanelNode
moveBy delta node = do
    let m      = node ^. _panelObject
        pos    = position m
        newPos = Vector.add pos delta

    setPosition newPos m
    pure $ panelUpdated $ node # _panel %~ addDelta delta

-- | update the panelnode's events after update panel value inside.
panelUpdated :: PanelNode -> PanelNode
panelUpdated pn = pn # _tapped  .~ (const p <$> m ^. _tapped)
                     # _dragged .~ (mkDragInfo p <$> m ^. _dragged)
    where p = pn ^. _panel
          m = pn ^. _panelObject

-- update panel mesh position based on array config and the corresponding panel model
updatePosition :: Panel -> ArrayConfig -> PanelNode -> Effect Unit
updatePosition p arrCfg = setPosition pv
    where px = meterVal $ p ^. _x
          py = meterVal $ p ^. _y
          z  = meterVal $ arrCfg ^. _panelLowestZ
          pv = case validatedSlope p of
                  Nothing -> mkVec3 px py z
                  Just slope -> let s = size p
                                    h = meterVal (s ^. _height) * 0.5 * sin slope
                                in mkVec3 px py (z + h)

-- update the rotation of the panel mesh
updateRotation :: Panel -> PanelNode -> Effect Unit
updateRotation p = rotateWithEuler euler
    where euler = case validatedSlope p of
                     Nothing -> rotateForNormal $ p ^. _orientation
                     Just slope -> rotateForFlat (p ^. _orientation) slope
          rotateForNormal Landscape = mkEuler 0.0 0.0 (pi / 2.0)
          rotateForNormal Portrait  = mkEuler 0.0 0.0 0.0

          rotateForFlat Landscape slope = mkEuler 0.0 (- radianVal slope) (pi / 2.0)
          rotateForFlat Portrait  slope = mkEuler (radianVal slope) 0.0 0.0

updateOpacity :: PanelOpacity -> PanelNode -> Effect Unit
updateOpacity opacity node = updateOpacityVal (opacityValue opacity) node

updateOpacityVal :: Number -> PanelNode -> Effect Unit
updateOpacityVal op node = traverse_ f (node ^. _materials)
    where f mat = setTransparent true mat *> setOpacity op mat

enableShadows :: Boolean -> PanelNode -> Effect Unit
enableShadows e node = setCastShadow e node

changeToNormal :: PanelNode -> Effect PanelNode
changeToNormal node = do
    -- change to use the normal panel material
    setMaterial (getPanelMaterial false $ node ^. _materialURL) (node ^. _panelObject <<< _mesh)
    updateOpacity Opaque node
    pure node
