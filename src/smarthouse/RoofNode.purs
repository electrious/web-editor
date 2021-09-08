module SmartHouse.RoofNode where

import Prelude

import Control.Plus (empty)
import Data.Default (class Default, def)
import Data.Lens ((.~), (^.))
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Traversable (traverse_)
import Data.Tuple (Tuple(..))
import Data.UUIDWrapper (UUID)
import Editor.Common.Lenses (_mouseMove, _name, _normal, _polygon, _tapped)
import Editor.SceneEvent (SceneMouseMoveEvent)
import Effect.Class (liftEffect)
import Effect.Unsafe (unsafePerformEffect)
import FRP.Dynamic (Dynamic, gateDyn)
import FRP.Event (Event)
import Model.ActiveMode (ActiveMode(..), fromBoolean)
import Model.Polygon (Polygon, polyOutline)
import Model.SmartHouse.HouseTextureInfo (HouseTextureInfo, _size, _texture)
import Model.SmartHouse.Roof (Roof, RoofState(..), roofState)
import Model.UUID (idLens)
import Rendering.Line (renderLineOnlyWith)
import Rendering.Node (Node, _exportable, getEnv, tapMesh, tapMouseMesh)
import SmartHouse.PolyGeometry (mkPolyGeometry, mkPolyGeometryWithUV)
import Three.Core.Material (LineBasicMaterial, MeshPhongMaterial, mkLineBasicMaterial, mkMeshBasicMaterialWithTexture, mkMeshPhongMaterial)
import Three.Math.Vector (Vector3)


newtype RoofMesh = RoofMesh {
    tapped    :: Event UUID,
    mouseMove :: Event (Tuple UUID SceneMouseMoveEvent)
}

derive instance Newtype RoofMesh _
instance Default RoofMesh where
    def = RoofMesh {
        tapped    : empty,
        mouseMove : empty
    }

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

renderRoof :: Dynamic Boolean -> Dynamic Boolean -> Roof -> Node HouseTextureInfo RoofMesh
renderRoof enableDyn chimEditDyn roof = do
    let poly = roof ^. _polygon
        rid  = roof ^. idLens
    -- render the roof polygon
    if roofState roof == Gable
        then renderGableRoof rid enableDyn poly (roof ^. _normal)
        else renderSlopeRoof rid enableDyn chimEditDyn poly


gableMat :: MeshPhongMaterial
gableMat = unsafePerformEffect $ mkMeshPhongMaterial 0x999999

renderGableRoof :: forall e. UUID -> Dynamic Boolean -> Polygon Vector3 -> Vector3 -> Node e RoofMesh
renderGableRoof rid enableDyn poly norm = do
    geo <- liftEffect $ mkPolyGeometry poly norm
    m <- tapMesh (def # _name       .~ "roof"
                      # _exportable .~ true) geo gableMat
    pure $ def # _tapped .~ gateDyn enableDyn (const rid <$> m ^. _tapped)

renderSlopeRoof :: UUID -> Dynamic Boolean -> Dynamic Boolean -> Polygon Vector3 -> Node HouseTextureInfo RoofMesh
renderSlopeRoof rid enableDyn chimEditDyn poly = do
    info <- getEnv
    geo <- liftEffect $ mkPolyGeometryWithUV (info ^. _size) poly
    mat <- liftEffect $ mkMeshBasicMaterialWithTexture (info ^. _texture)
    m <- tapMouseMesh (def # _name       .~ "roof"
                           # _exportable .~ true) geo mat
    pure $ def # _tapped    .~ gateDyn enableDyn (const rid <$> m ^. _tapped)
               # _mouseMove .~ gateDyn ((&&) <$> enableDyn <*> chimEditDyn) (Tuple rid <$> m ^. _mouseMove)
