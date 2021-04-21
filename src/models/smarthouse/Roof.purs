module Model.SmartHouse.Roof where

import Prelude

import Data.Default (def)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Lens (Lens', (.~), (^.))
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.List (List, length)
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy(..))
import Data.Traversable (traverse_)
import Data.UUID (UUID, genUUID)
import Editor.Common.Lenses (_id, _name, _polygon, _tapped)
import Effect (Effect)
import Effect.Class (liftEffect)
import FRP.Event (Event)
import Model.Polygon (Polygon, polyOutline)
import Model.SmartHouse.HouseTextureInfo (HouseTextureInfo, _size, _texture)
import Model.UUID (class HasUUID, idLens)
import Rendering.Node (Node, getEnv, tapMesh)
import SmartHouse.HouseTracer (renderLine)
import SmartHouse.PolyGeometry (mkPolyGeometryWithUV)
import Smarthouse.Algorithm.Subtree (IndexedSubtree)
import Three.Core.Material (mkMeshBasicMaterialWithTexture)
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

canBeGable :: Roof -> Boolean
canBeGable r = length (r ^. _subtrees) < 2

renderRoof :: Roof -> Node HouseTextureInfo (Event UUID)
renderRoof roof = do
    info <- getEnv

    let poly = roof ^. _polygon
    -- render the roof outline as white line
    traverse_ renderLine $ polyOutline poly
    
    -- render the roof polygon
    geo <- liftEffect $ mkPolyGeometryWithUV (info ^. _size) poly
    mat <- liftEffect $ mkMeshBasicMaterialWithTexture (info ^. _texture)
    m <- tapMesh (def # _name .~ "roof") geo mat
    pure $ const (roof ^. idLens) <$> m ^. _tapped
