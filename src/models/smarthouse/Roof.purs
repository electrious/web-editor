module Model.SmartHouse.Roof where

import Prelude

import Control.Alternative (empty)
import Custom.Mesh (TappableMesh)
import Data.Compactable (compact)
import Data.Default (class Default, def)
import Data.Enum (fromEnum)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Lens (Lens', (.~), (^.))
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.List (List(..), (:))
import Data.Map as M
import Data.Maybe (Maybe(..))
import Data.Meter (Meter, meterVal)
import Data.Newtype (class Newtype)
import Data.Set (Set)
import Data.Symbol (SProxy(..))
import Data.Traversable (class Traversable, traverse_)
import Data.UUID (UUID, genUUID)
import Data.UUIDMap (UUIDMap)
import Data.UUIDMap as UM
import Editor.Common.Lenses (_id, _name, _normal, _polygon, _shade, _tapped)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Unsafe (unsafePerformEffect)
import FRP.Dynamic (Dynamic, gateDyn)
import FRP.Event (Event)
import FRP.Event.Extra (multicast)
import Foreign.Class (class Decode, class Encode)
import Foreign.Generic (defaultOptions, genericDecode, genericEncode)
import Model.ActiveMode (ActiveMode(..), fromBoolean, isActive)
import Model.Polygon (Polygon, _polyVerts, polyOutline)
import Model.Roof.RoofPlate (Point, vec2Point)
import Model.SmartHouse.HouseTextureInfo (HouseTextureInfo, _size, _texture)
import Model.UUID (class HasUUID, idLens)
import Rendering.Node (Node, getEnv, tapMesh)
import SmartHouse.BuilderMode (BuilderMode(..))
import SmartHouse.HouseTracer (lineMat, renderLineWith)
import SmartHouse.PolyGeometry (mkPolyGeometry, mkPolyGeometryWithUV)
import SmartHouse.ShadeOption (ShadeOption(..))
import Smarthouse.Algorithm.Subtree (Subtree, _isGable)
import Three.Core.Material (LineBasicMaterial, MeshPhongMaterial, mkLineBasicMaterial, mkMeshBasicMaterialWithTexture, mkMeshPhongMaterial)
import Three.Math.Vector (Vector3, mkVec3, vecX, vecY, vecZ)

data RoofState = SlopeRoof
               | Gable

derive instance eqRoofState :: Eq RoofState

newtype Roof = Roof {
    id       :: UUID,
    polygon  :: Polygon Vector3,
    subtrees :: UUIDMap Subtree,

    shade    :: ShadeOption,

    normal   :: Vector3
    }

derive instance newtypeRoof :: Newtype Roof _
derive instance genericRoof :: Generic Roof _
instance showRoof :: Show Roof where
    show = genericShow
instance eqRoof :: Eq Roof where
    eq r1 r2 = r1 ^. idLens == r2 ^. idLens
instance hasUUIDRoof :: HasUUID Roof where
    idLens = _id

_subtrees :: forall t a r. Newtype t { subtrees :: a | r } => Lens' t a
_subtrees = _Newtype <<< prop (SProxy :: SProxy "subtrees")

createRoofFrom :: Polygon Vector3 -> Set Subtree -> Vector3 -> Effect Roof
createRoofFrom p ts n = do
    i <- genUUID
    pure $ Roof { id : i, polygon : p, subtrees : UM.fromSet ts, shade : NoShade, normal : n }

-- check if a roof can be gable
canBeGable :: Roof -> Boolean
canBeGable r = M.size (r ^. _subtrees) < 2

-- | get the roof's current state, if it's gable or not
roofState :: Roof -> RoofState
roofState r = case M.values $ r ^. _subtrees of
    (t:Nil) -> if t ^. _isGable then Gable else SlopeRoof
    _       -> SlopeRoof

subtreeIndex :: Roof -> Maybe UUID
subtreeIndex r = case M.values $ r ^. _subtrees of
    (t:Nil) -> Just $ t ^. idLens
    _       -> Nothing


updateShadeOption :: Roof -> ShadeOption -> Roof
updateShadeOption r o = r # _shade .~ o


exportRoof :: Meter -> Roof -> JSRoof
exportRoof h r = JSRoof { id: r ^. idLens, polygon: mkP <$> r ^. _polygon <<< _polyVerts, shade : fromEnum (r ^. _shade) }
    where hv = meterVal h
          mkP v = vec2Point $ mkVec3 (vecX v) (vecY v) (vecZ v + hv)


-- The Roof data structure saved to server
newtype JSRoof = JSRoof {
    id      :: UUID,
    polygon :: Array Point,
    shade   :: Int
    }

derive instance genericJSRoof :: Generic JSRoof _
instance showJSRoof :: Show JSRoof where
    show = genericShow
instance encodeJSRoof :: Encode JSRoof where
    encode = genericEncode (defaultOptions { unwrapSingleConstructors = true })
instance decodeJSRoof :: Decode JSRoof where
    decode = genericDecode (defaultOptions { unwrapSingleConstructors = true })

newtype RoofEvents = RoofEvents {
    tapped  :: Event UUID,
    flipped :: Event UUID   -- event to flip the roof state, the Int is index of the subtree
    }

derive instance newtypeRoofEvents :: Newtype RoofEvents _
instance defaultRoofEvents :: Default RoofEvents where
    def = RoofEvents {
        tapped  : empty,
        flipped : empty
        }

_flipped :: forall t a r. Newtype t { flipped :: a | r } => Lens' t a
_flipped = _Newtype <<< prop (SProxy :: SProxy "flipped")

-- material for active roof outline
actLineMat :: LineBasicMaterial
actLineMat = unsafePerformEffect $ mkLineBasicMaterial 0xeeee00 4.0

getRoofLineMat :: ActiveMode -> LineBasicMaterial
getRoofLineMat Active   = actLineMat
getRoofLineMat Inactive = lineMat

getRoofActive :: Roof -> Maybe UUID -> ActiveMode
getRoofActive r (Just i) = fromBoolean $ i == r ^. idLens
getRoofActive _ Nothing  = Inactive

renderRoofOutline :: forall e. Maybe UUID -> Roof -> Node e Unit
renderRoofOutline actId r = render $ getRoofLineMat $ getRoofActive r actId
    where render m = traverse_ (flip renderLineWith m) (polyOutline $ r ^. _polygon)

renderRoofOutlines :: forall e f. Traversable f => BuilderMode -> Maybe UUID -> f Roof -> Node e Unit
renderRoofOutlines Building actId = traverse_ (renderRoofOutline actId)
renderRoofOutlines Showing    _   = const (pure unit)

renderRoof :: Dynamic Boolean -> Dynamic (Maybe UUID) -> Roof -> Node HouseTextureInfo RoofEvents
renderRoof enableDyn actIdDyn roof = do
    let poly  = roof ^. _polygon
        gable = canBeGable roof

        actDyn = getRoofActive roof <$> actIdDyn

    -- render the roof polygon
    m <- if roofState roof == Gable
         then renderGableRoof poly (roof ^. _normal)
         else renderSlopeRoof poly
    let tapEvt      = multicast $ m ^. _tapped

        actTapEvt   = gateDyn ((&&) <$> enableDyn <*> (isActive <$> actDyn)) tapEvt
        inactTapEvt = gateDyn ((&&) <$> enableDyn <*> (not <<< isActive <$> actDyn)) tapEvt
        
    pure $ def # _tapped  .~ multicast (const (roof ^. idLens) <$> inactTapEvt)
               # _flipped .~ multicast (compact $ const (subtreeIndex roof) <$> actTapEvt)


gableMat :: MeshPhongMaterial
gableMat = unsafePerformEffect $ mkMeshPhongMaterial 0x999999

renderGableRoof :: forall e. Polygon Vector3 -> Vector3 -> Node e TappableMesh
renderGableRoof poly norm = do
    geo <- liftEffect $ mkPolyGeometry poly norm
    tapMesh (def # _name .~ "roof") geo gableMat

renderSlopeRoof :: Polygon Vector3 -> Node HouseTextureInfo TappableMesh
renderSlopeRoof poly = do
    info <- getEnv
    geo <- liftEffect $ mkPolyGeometryWithUV (info ^. _size) poly
    mat <- liftEffect $ mkMeshBasicMaterialWithTexture (info ^. _texture)
    tapMesh (def # _name .~ "roof") geo mat
