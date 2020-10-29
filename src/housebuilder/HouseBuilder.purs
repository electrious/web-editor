module HouseBuilder.HouseBuilder where

import Prelude hiding (add)

import Control.Monad.Reader (class MonadAsk, class MonadReader, ReaderT, ask, runReaderT)
import Custom.Mesh (TapDragMesh, mkTapDragMesh)
import Data.Default (class Default, def)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Lens (Lens', (^.))
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy(..))
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Rendering.TextureLoader (loadTextureFromUrl)
import Three.Core.Geometry (mkPlaneGeometry)
import Three.Core.Material (mkMeshBasicMaterial, mkMeshBasicMaterialWithTexture)
import Three.Core.Object3D (add, mkObject3D, setName)

-- represent the state of the builder
data BuilderMode = AddGutter
                 | AddRidge
                 | LiftRidge
                 | LiftHouse

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


-- HouseBuilder is the main Monad where house builder functionalities happen
newtype HouseBuilder a = HouseBuilder (ReaderT HouseBuilderConfig Effect a)

derive newtype instance functorHouseBuilder :: Functor HouseBuilder
derive newtype instance applyHouseBuilder :: Apply HouseBuilder
derive newtype instance applicativeHouseBuilder :: Applicative HouseBuilder
derive newtype instance bindHouseBuilder :: Bind HouseBuilder
derive newtype instance monadHouseBuilder :: Monad HouseBuilder
derive newtype instance monadAskHouseBuilder :: MonadAsk HouseBuilderConfig HouseBuilder
derive newtype instance monadReaderHouseBuilder :: MonadReader HouseBuilderConfig HouseBuilder
derive newtype instance monadEFfectHouseBuilder :: MonadEffect HouseBuilder

runHouseBuilder :: forall a. HouseBuilder a -> Effect a
runHouseBuilder (HouseBuilder b) = do
    let cfg = def
    runReaderT b cfg


mkHelperPlane :: HouseBuilder TapDragMesh
mkHelperPlane = do
    cfg <- ask
    let img = cfg ^. _image

    geo <- liftEffect $ mkPlaneGeometry 100.0 100.0 10 10
    mat <- liftEffect $ if img == ""
                        then mkMeshBasicMaterial 0x002222
                        else mkMeshBasicMaterialWithTexture $ loadTextureFromUrl img

    m <- liftEffect $ mkTapDragMesh geo mat
    liftEffect $ setName "helper-plane" m
    pure m

createHouseBuilder :: HouseBuilder Unit
createHouseBuilder = do
    builder <- liftEffect mkObject3D
    liftEffect $ setName "house-builder" builder

    -- add helper plane that accepts tap and drag events
    plane <- mkHelperPlane
    liftEffect $ add plane builder
