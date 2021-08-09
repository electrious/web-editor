module Model.SmartHouse.Roof where

import Prelude hiding (degree)

import Algorithm.Plane (Plane)
import Custom.Mesh (TappableMesh)
import Data.Default (def)
import Data.Generic.Rep (class Generic)
import Data.Lens (view, (.~), (^.))
import Data.List (List)
import Data.Maybe (Maybe(..))
import Data.Meter (Meter, meterVal)
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
import Data.Traversable (traverse_)
import Data.UUID (UUID)
import Editor.Common.Lenses (_id, _name, _normal, _polygon, _slope, _tapped)
import Effect.Class (liftEffect)
import Effect.Unsafe (unsafePerformEffect)
import FRP.Dynamic (Dynamic, gateDyn)
import FRP.Event (Event)
import FRP.Event.Extra (multicast)
import Foreign.Class (class Decode, class Encode)
import Foreign.Generic (defaultOptions, genericDecode, genericEncode)
import Math.Angle (Angle, degree)
import Model.ActiveMode (ActiveMode(..), fromBoolean)
import Model.Polygon (Polygon, _polyVerts, polyOutline, polyPlane)
import Model.Roof.RoofPlate (Point, vec2Point)
import Model.SmartHouse.HouseTextureInfo (HouseTextureInfo, _size, _texture)
import Model.UUID (class HasUUID, idLens)
import Rendering.Line (renderLineOnlyWith)
import Rendering.Node (Node, _exportable, getEnv, tapMesh)
import SmartHouse.Algorithm.Edge (Edge)
import SmartHouse.PolyGeometry (mkPolyGeometry, mkPolyGeometryWithUV)
import Three.Core.Material (LineBasicMaterial, MeshPhongMaterial, mkLineBasicMaterial, mkMeshBasicMaterialWithTexture, mkMeshPhongMaterial)
import Three.Math.Vector (Vector3, mkVec3, vecX, vecY, vecZ)

data RoofState = SlopeRoof
               | Gable

derive instance Eq RoofState

newtype Roof = Roof {
    id       :: UUID,
    polygon  :: Polygon Vector3,

    edges    :: List Edge,
    slope    :: Angle,

    normal   :: Vector3
    }

derive instance Newtype Roof _
derive instance Generic Roof _
instance Show Roof where
    show = genericShow
instance Eq Roof where
    eq r1 r2 = r1 ^. idLens == r2 ^. idLens
instance HasUUID Roof where
    idLens = _id

createRoofFrom :: UUID -> Polygon Vector3 -> List Edge -> Vector3 -> Angle -> Roof
createRoofFrom i p es n s = Roof { id : i, polygon : p, edges : es, slope: s, normal : n }

roofState :: Roof -> RoofState
roofState r = if r ^. _slope > degree 89.0 then Gable else SlopeRoof

roofPlane :: Roof -> Plane
roofPlane = polyPlane <<< view _polygon

exportRoof :: Meter -> Roof -> JSRoof
exportRoof h r = JSRoof { id: r ^. idLens, polygon: mkP <$> r ^. _polygon <<< _polyVerts }
    where hv = meterVal h
          mkP v = vec2Point $ mkVec3 (vecX v) (vecY v) (vecZ v + hv)


-- The Roof data structure saved to server
newtype JSRoof = JSRoof {
    id      :: UUID,
    polygon :: Array Point
    }

derive instance Generic JSRoof _
instance Show JSRoof where
    show = genericShow
instance Encode JSRoof where
    encode = genericEncode (defaultOptions { unwrapSingleConstructors = true })
instance Decode JSRoof where
    decode = genericDecode (defaultOptions { unwrapSingleConstructors = true })

-- material for active roof outline
actLineMat :: LineBasicMaterial
actLineMat = unsafePerformEffect $ mkLineBasicMaterial 0xeeee00 4.0

getRoofActive :: Roof -> Maybe UUID -> ActiveMode
getRoofActive r (Just i) = fromBoolean $ i == r ^. idLens
getRoofActive _ Nothing  = Inactive

renderRoofOutline :: forall e. Roof -> Node e Unit
renderRoofOutline r = traverse_ (flip renderLineOnlyWith actLineMat) (polyOutline $ r ^. _polygon)

renderActRoofOutline :: forall e. Maybe Roof -> Node e Unit
renderActRoofOutline (Just r) = renderRoofOutline r
renderActRoofOutline Nothing  = pure unit

renderRoof :: Dynamic Boolean -> Roof -> Node HouseTextureInfo (Event UUID)
renderRoof enableDyn roof = do
    let poly  = roof ^. _polygon

    -- render the roof polygon
    m <- if roofState roof == Gable
         then renderGableRoof poly (roof ^. _normal)
         else renderSlopeRoof poly
    let tapEvt    = m ^. _tapped
        actTapEvt = gateDyn enableDyn tapEvt
    pure $ multicast (const (roof ^. idLens) <$> actTapEvt)

gableMat :: MeshPhongMaterial
gableMat = unsafePerformEffect $ mkMeshPhongMaterial 0x999999

renderGableRoof :: forall e. Polygon Vector3 -> Vector3 -> Node e TappableMesh
renderGableRoof poly norm = do
    geo <- liftEffect $ mkPolyGeometry poly norm
    tapMesh (def # _name       .~ "roof"
                 # _exportable .~ true) geo gableMat

renderSlopeRoof :: Polygon Vector3 -> Node HouseTextureInfo TappableMesh
renderSlopeRoof poly = do
    info <- getEnv
    geo <- liftEffect $ mkPolyGeometryWithUV (info ^. _size) poly
    mat <- liftEffect $ mkMeshBasicMaterialWithTexture (info ^. _texture)
    tapMesh (def # _name       .~ "roof"
                 # _exportable .~ true) geo mat
