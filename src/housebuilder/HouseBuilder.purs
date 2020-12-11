module HouseBuilder.HouseBuilder where

import Prelude hiding (add)

import Control.Plus (empty)
import Custom.Mesh (TapDragMesh)
import Data.Default (class Default, def)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Lens (Lens', view, (.~))
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple(..), snd)
import Editor.Common.Lenses (_mouseMove, _name)
import Editor.SceneEvent (SceneMouseMoveEvent, makeMouseMove, stopMouseMove)
import Effect.Class (liftEffect)
import FRP.Dynamic (step)
import FRP.Event (Event, makeEvent)
import FRP.Event.Extra (multicast)
import HouseBuilder.FloorPlanBuilder (_canEdit, buildFloorPlan)
import Rendering.Node (Node, getEnv, leaf, node, tapDragMesh)
import Rendering.TextureLoader (loadTextureFromUrl)
import Three.Core.Geometry (mkPlaneGeometry)
import Three.Core.Material (mkMeshBasicMaterial, mkMeshBasicMaterialWithTexture)

-- represent the state of the builder
data BuilderMode = AddGutter  -- Add gutter points and lines
                 | AddRidge   -- Add ridge points and lines
                 | LiftRidge  -- lift ridges to the right height
                 | LiftHouse  -- lift house to the right height

derive instance genericBuilderMode :: Generic BuilderMode _
derive instance eqBuilderMode :: Eq BuilderMode
instance showBuilderMode :: Show BuilderMode where
    show = genericShow


newtype HouseBuilderConfig = HouseBuilderConfig {
    image :: String
}

derive instance genericHouseBuilderConfig :: Generic HouseBuilderConfig _
derive instance newtypeHouseBuilderConfig :: Newtype HouseBuilderConfig _
instance showHouseBuilderConfig :: Show HouseBuilderConfig where
    show = genericShow
instance defaultHouseBuilderConfig :: Default HouseBuilderConfig where
    def = HouseBuilderConfig { image : "" }

_image :: forall t a r. Newtype t { image :: a | r } => Lens' t a
_image = _Newtype <<< prop (SProxy :: SProxy "image")


mkHelperPlane :: Node HouseBuilderConfig (Tuple TapDragMesh (Event SceneMouseMoveEvent))
mkHelperPlane = do
    img <- view _image <$> getEnv

    geo <- liftEffect $ mkPlaneGeometry 100.0 100.0 10 10
    mat <- liftEffect $ if img == ""
                        then mkMeshBasicMaterial 0x002222
                        else mkMeshBasicMaterialWithTexture $ loadTextureFromUrl img

    m <- snd <$> tapDragMesh (def # _name .~ "helper-plane") geo mat leaf
    let mouseEvt = multicast $ makeEvent \k -> do
            makeMouseMove m k
            pure (stopMouseMove m)
            
    pure $ Tuple m mouseEvt

createHouseBuilder :: Node HouseBuilderConfig Unit
createHouseBuilder = node (def # _name .~ "house-builder") do
    -- add helper plane that accepts tap and drag events
    Tuple plane mouseEvt <- mkHelperPlane

    floorPlanEvt <- buildFloorPlan $ def # _mouseMove .~ mouseEvt
                                         # _canEdit .~ step true empty
    
    pure unit
