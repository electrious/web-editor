module Model.SmartHouse.Roof where

import Prelude

import Custom.Mesh (TappableMesh)
import Data.Compactable (compact)
import Data.Default (def)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Lens (Lens', (.~), (^.))
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.List (List(..), (:), length)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy(..))
import Data.Traversable (traverse_)
import Data.UUID (UUID, genUUID)
import Editor.Common.Lenses (_id, _name, _polygon, _tapped)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Unsafe (unsafePerformEffect)
import FRP.Dynamic (Dynamic, gateDyn)
import FRP.Event (Event)
import FRP.Event.Extra (multicast)
import Model.ActiveMode (ActiveMode, isActive)
import Model.Polygon (Polygon, polyOutline)
import Model.SmartHouse.HouseTextureInfo (HouseTextureInfo, _size, _texture)
import Model.UUID (class HasUUID, idLens)
import Rendering.Node (Node, getEnv, tapMesh)
import SmartHouse.HouseTracer (renderLine)
import SmartHouse.PolyGeometry (mkPolyGeometry, mkPolyGeometryWithUV)
import Smarthouse.Algorithm.Subtree (IndexedSubtree, _isGable, getIndex, getSubtree)
import Three.Core.Material (MeshBasicMaterial, mkMeshBasicMaterial, mkMeshBasicMaterialWithTexture)
import Three.Math.Vector (Vector3)


data RoofState = SlopeRoof
               | Gable

derive instance eqRoofState :: Eq RoofState

newtype Roof = Roof {
    id       :: UUID,
    polygon  :: Polygon Vector3,
    subtrees :: List IndexedSubtree
    }

derive instance newtypeRoof :: Newtype Roof _
derive instance genericRoof :: Generic Roof _
instance showRoof :: Show Roof where
    show = genericShow
instance hasUUIDRoof :: HasUUID Roof where
    idLens = _id

_subtrees :: forall t a r. Newtype t { subtrees :: a | r } => Lens' t a
_subtrees = _Newtype <<< prop (SProxy :: SProxy "subtrees")

createRoofFrom :: Polygon Vector3 -> List IndexedSubtree -> Effect Roof
createRoofFrom p ts = do
    i <- genUUID
    pure $ Roof { id : i, polygon : p, subtrees : ts }

-- check if a roof can be gable
canBeGable :: Roof -> Boolean
canBeGable r = length (r ^. _subtrees) < 2

-- | get the roof's current state, if it's gable or not
roofState :: Roof -> RoofState
roofState r = case r ^. _subtrees of
    (t:Nil) -> if (getSubtree t) ^. _isGable then Gable else SlopeRoof
    _       -> SlopeRoof

subtreeIndex :: Roof -> Maybe Int
subtreeIndex r = case r ^. _subtrees of
    (t:Nil) -> Just $ getIndex t
    _       -> Nothing


newtype RoofEvents = RoofEvents {
    tapped  :: Event UUID,
    flipped :: Event Int   -- event to flip the roof state, the Int is index of the subtree
    }

derive instance newtypeRoofEvents :: Newtype RoofEvents _

_flipped :: forall t a r. Newtype t { flipped :: a | r } => Lens' t a
_flipped = _Newtype <<< prop (SProxy :: SProxy "flipped")

renderRoof :: Dynamic ActiveMode -> Roof -> Node HouseTextureInfo RoofEvents
renderRoof actDyn roof = do
    let poly  = roof ^. _polygon
        gable = canBeGable roof
    -- render the roof outline as white line
    traverse_ renderLine $ polyOutline poly

    -- render the roof polygon
    m <- if roofState roof == Gable
         then renderGableRoof poly
         else renderSlopeRoof poly
    let tapEvt      = multicast $ m ^. _tapped
        actTapEvt   = gateDyn (isActive <$> actDyn) tapEvt
        inactTapEvt = gateDyn (not <<< isActive <$> actDyn) tapEvt
        
    pure $ RoofEvents {
        tapped  : const (roof ^. idLens) <$> inactTapEvt,
        flipped : compact $ const (subtreeIndex roof) <$> actTapEvt
        }


-- NOTE: MeshPhongMaterial doesn't work here in mobile Safari, though it works for the walls.
gableMat :: MeshBasicMaterial
gableMat = unsafePerformEffect $ mkMeshBasicMaterial 0x999999

renderGableRoof :: forall e. Polygon Vector3 -> Node e TappableMesh
renderGableRoof poly = do
    geo <- liftEffect $ mkPolyGeometry poly
    tapMesh (def # _name .~ "roof") geo gableMat

renderSlopeRoof :: Polygon Vector3 -> Node HouseTextureInfo TappableMesh
renderSlopeRoof poly = do
    info <- getEnv
    geo <- liftEffect $ mkPolyGeometryWithUV (info ^. _size) poly
    mat <- liftEffect $ mkMeshBasicMaterialWithTexture (info ^. _texture)
    tapMesh (def # _name .~ "roof") geo mat
