module Model.SmartHouse.Roof where

import Prelude

import Data.Default (def)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Lens ((.~), (^.))
import Data.Newtype (class Newtype)
import Data.Traversable (traverse_)
import Data.UUID (UUID, genUUID)
import Editor.Common.Lenses (_id, _name, _polygon, _tapped)
import Effect (Effect)
import Effect.Class (liftEffect)
import FRP.Event (Event)
import SmartHouse.PolyGeometry (mkPolyGeometryWithUV)
import Model.Polygon (Polygon, polyOutline)
import Model.SmartHouse.HouseTextureInfo (HouseTextureInfo, _size, _texture)
import Model.UUID (class HasUUID, idLens)
import Rendering.Node (Node, getEnv, tapMesh)
import SmartHouse.HouseTracer (renderLine)
import Three.Core.Material (mkMeshBasicMaterialWithTexture)
import Three.Math.Vector (Vector3)

newtype Roof = Roof {
    id      :: UUID,
    polygon :: Polygon Vector3
    }

derive instance newtypeRoof :: Newtype Roof _
derive instance genericRoof :: Generic Roof _
instance showRoof :: Show Roof where
    show = genericShow
instance hasUUIDRoof :: HasUUID Roof where
    idLens = _id

createRoofFrom :: Polygon Vector3 -> Effect Roof
createRoofFrom p = do
    i <- genUUID
    pure $ Roof { id : i, polygon : p }

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
