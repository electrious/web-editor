module HouseBuilder.HouseBuilder where

import Prelude hiding (add)

import Control.Monad.Cont (lift)
import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Custom.Mesh (TapDragMesh)
import Data.Default (class Default, def)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Lens (Lens', view, (.~))
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy(..))
import Data.Tuple (snd)
import Editor.Common.Lenses (_name)
import Effect.Class (liftEffect)
import HouseBuilder.FloorPlanBuilder (buildFloorPlan)
import Rendering.Node (Node, Props, leaf, node, tapDragMesh)
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


-- HouseBuilder is the main Monad where house building happens
type HouseBuilder e a = ReaderT HouseBuilderConfig (Node e) a

runHouseBuilder :: forall e a. HouseBuilder e a -> HouseBuilderConfig -> Node e a
runHouseBuilder = runReaderT

-- | lift a Node transform to HouseBuilder transform
liftNodeFunc :: forall e a b. (Node e a -> Node e b) -> HouseBuilder e a -> HouseBuilder e b
liftNodeFunc f child = do
    cfg <- ask
    lift $ f $ runHouseBuilder child cfg

nodeBuilder :: forall e a. Props -> HouseBuilder e a -> HouseBuilder e a
nodeBuilder p = liftNodeFunc (node p)

mkHelperPlane :: forall e. HouseBuilder e TapDragMesh
mkHelperPlane = do
    img <- view _image <$> ask

    geo <- liftEffect $ mkPlaneGeometry 100.0 100.0 10 10
    mat <- liftEffect $ if img == ""
                        then mkMeshBasicMaterial 0x002222
                        else mkMeshBasicMaterialWithTexture $ loadTextureFromUrl img

    m <- lift $ snd <$> tapDragMesh (def # _name .~ "helper-plane") geo mat leaf
    pure m

createHouseBuilder :: forall e. HouseBuilder e Unit
createHouseBuilder = nodeBuilder (def # _name .~ "house-builder") do
    -- add helper plane that accepts tap and drag events
    plane <- mkHelperPlane

    floorPlanEvt <- lift $ buildFloorPlan def
    
    pure unit
