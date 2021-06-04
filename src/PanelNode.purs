module Editor.PanelNode where

import Prelude hiding (add)

import Data.Default (def)
import Data.Enum (class BoundedEnum, class Enum)
import Data.Foldable (traverse_)
import Data.Function.Memoize (class Tabulate, genericTabulate, memoize2)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Bounded (genericBottom, genericTop)
import Data.Generic.Rep.Enum (genericCardinality, genericFromEnum, genericPred, genericSucc, genericToEnum)
import Data.Generic.Rep.Show (genericShow)
import Data.Lens (Lens', (.~), (^.))
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Meter (meterVal)
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy(..))
import Data.Traversable (class Traversable)
import Editor.Common.Lenses (_dragged, _height, _name, _orientation, _position, _rackingType, _rotation, _tapped, _x, _y)
import Editor.Rendering.DefMaterials (loadMaterial)
import Editor.UI.DragInfo (DragInfo, mkDragInfo)
import Effect (Effect)
import Effect.Class (liftEffect)
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
import Rendering.Node (Node, _renderOrder, mesh, node, tapDragMesh)
import Three.Core.Geometry (class IsGeometry, BoxGeometry, mkBoxGeometry)
import Three.Core.Material (class IsMaterial, MeshBasicMaterial, mkMeshBasicMaterialWithTexture, setOpacity, setTransparent)
import Three.Core.Mesh (Mesh)
import Three.Loader.TextureLoader (loadTexture, mkTextureLoader)
import Three.Math.Euler (Euler, mkEuler)
import Three.Math.Vector (Vector3, mkVec3)

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

-- | panel body's geometry
panelGeometry :: BoxGeometry
panelGeometry = unsafePerformEffect $ mkBoxGeometry (meterVal panelShort) (meterVal panelLong) 0.04

-- | panel vertical frame
verticalGeometry :: BoxGeometry
verticalGeometry = unsafePerformEffect $ mkBoxGeometry 0.01 (meterVal panelLong) 0.05

-- | panel horizontal frame
horizontalGeometry :: BoxGeometry
horizontalGeometry = unsafePerformEffect $ mkBoxGeometry (meterVal panelShort) 0.01 0.05

newtype PanelNode = PanelNode {
    panel       :: Panel,
    tapped      :: Event Panel,
    dragged     :: Event (DragInfo Panel),
    materials   :: List MeshBasicMaterial,
    materialURL :: String
}

derive instance newtypePanelNode :: Newtype PanelNode _

_panel :: forall t a r. Newtype t { panel :: a | r } => Lens' t a
_panel = _Newtype <<< prop (SProxy :: SProxy "panel")

_materials :: forall t a r. Newtype t { materials :: a | r } => Lens' t a
_materials = _Newtype <<< prop (SProxy :: SProxy "materials")

_materialURL :: forall t a r. Newtype t { materialURL :: a | r } => Lens' t a
_materialURL = _Newtype <<< prop (SProxy :: SProxy "materialURL")

mkTopFrame :: forall e geo mat. IsGeometry geo => IsMaterial mat => geo -> mat -> Node e Mesh
mkTopFrame = mesh (def # _name        .~ "top"
                       # _position    .~ pure (mkVec3 0.0 (meterVal panelLong / 2.0) 0.0)
                       # _renderOrder .~ 9
                  )

mkBotFrame :: forall e geo mat. IsGeometry geo => IsMaterial mat => geo -> mat -> Node e Mesh
mkBotFrame = mesh (def # _name        .~ "bottom"
                       # _position    .~ pure (mkVec3 0.0 (- meterVal panelLong / 2.0) 0.0)
                       # _renderOrder .~ 9
                  )

mkLeftFrame :: forall e geo mat. IsGeometry geo => IsMaterial mat => geo -> mat -> Node e Mesh
mkLeftFrame = mesh (def # _name        .~ "left"
                        # _position    .~ pure (mkVec3 (- meterVal panelShort / 2.0) 0.0 0.0)
                        # _renderOrder .~ 9
                   )

mkRightFrame :: forall e geo mat. IsGeometry geo => IsMaterial mat => geo -> mat -> Node e Mesh
mkRightFrame = mesh (def # _name        .~ "right"
                         # _position    .~ pure (mkVec3 (meterVal panelShort / 2.0) 0.0 0.0)
                         # _renderOrder .~ 9
                    )

-- | make a default panel mesh node
mkPanelNode :: forall e. ArrayConfig -> PanelTextureInfo -> PanelType -> Panel -> Boolean -> Node e PanelNode
mkPanelNode arrCfg info panelType p isTemp = 
    node (def # _name     .~ "panel"
              # _position .~ pure (calcPosition p arrCfg)
              # _rotation .~ pure (calcRotation p)
         ) do

        let -- always use black material for frame around panel
            blackMat = loadMaterial Premium

            rackingType = arrCfg ^. _rackingType
            matUrl      = materialUrl info $ panelTextureType rackingType panelType
            -- use different material for normal and temp panels
            bodyMat     = getPanelMaterial isTemp matUrl

        when isTemp $ liftEffect $ updateOpacity Transparent [bodyMat, blackMat]
        
        m <- tapDragMesh def panelGeometry bodyMat
        let pn = PanelNode {
                panel       : p,
                tapped      : const p <$> m ^. _tapped,
                dragged     : mkDragInfo p <$> m ^. _dragged,
                materials   : (bodyMat : blackMat : Nil),
                materialURL : matUrl
                }
        -- create frames
        void $ mkTopFrame horizontalGeometry blackMat
        void $ mkBotFrame horizontalGeometry blackMat
        void $ mkLeftFrame verticalGeometry blackMat
        void $ mkRightFrame verticalGeometry blackMat

        pure pn

{-
moveBy :: Vector3 -> PanelNode -> Effect PanelNode
moveBy delta node = do
    let m      = node ^. _panelObject
        pos    = position m
        newPos = Vector.add pos delta

    setPosition newPos m
    pure $ panelUpdated $ node # _panel %~ addDelta delta
-}

-- update panel mesh position based on array config and the corresponding panel model
calcPosition :: Panel -> ArrayConfig -> Vector3
calcPosition p arrCfg = case validatedSlope p of
    Nothing -> mkVec3 px py z
    Just slope -> let s = size p
                      h = meterVal (s ^. _height) * 0.5 * sin slope
                  in mkVec3 px py (z + h)
    where px = meterVal $ p ^. _x
          py = meterVal $ p ^. _y
          z  = meterVal $ arrCfg ^. _panelLowestZ

-- update the rotation of the panel mesh
calcRotation :: Panel -> Euler
calcRotation p = case validatedSlope p of
    Nothing -> rotateForNormal $ p ^. _orientation
    Just slope -> rotateForFlat (p ^. _orientation) slope
    where rotateForNormal Landscape = mkEuler 0.0 0.0 (pi / 2.0)
          rotateForNormal Portrait  = mkEuler 0.0 0.0 0.0

          rotateForFlat Landscape slope = mkEuler 0.0 (- radianVal slope) (pi / 2.0)
          rotateForFlat Portrait  slope = mkEuler (radianVal slope) 0.0 0.0

updateOpacity :: forall f mat. Traversable f => IsMaterial mat => PanelOpacity -> f mat -> Effect Unit
updateOpacity opacity = updateOpacityVal (opacityValue opacity)

updateOpacityVal :: forall f mat. Traversable f => IsMaterial mat => Number -> f mat -> Effect Unit
updateOpacityVal op = traverse_ f
    where f mat = setTransparent true mat *> setOpacity op mat
