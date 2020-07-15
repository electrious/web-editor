module Editor.HouseEditor where

import Prelude

import API (API, APIConfig, runAPI)
import Control.Monad.Reader (class MonadAsk, class MonadReader, ReaderT, ask, runReaderT)
import Control.Plus (empty)
import Data.Default (class Default, def)
import Data.Lens (Lens', view)
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy(..))
import Editor.EditorMode (EditorMode(..))
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import FRP.Dynamic (Dynamic, current, dynEvent, step)
import FRP.Event (Event, makeEvent, subscribe)
import Model.Hardware.PanelTextureInfo (PanelTextureInfo)
import Model.Hardware.PanelType (PanelType(..))
import Model.Roof.Panel (Panel)
import Model.Roof.RoofPlate (RoofPlate)

newtype HouseConfig = HouseConfig {
    leadId      :: Int,
    houseId     :: Int,
    roofPlates  :: Array RoofPlate,
    panels      :: Array Panel,
    dataServer  :: String,
    modeDyn     :: Dynamic EditorMode,
    textureInfo :: PanelTextureInfo,
    panelType   :: Dynamic PanelType,
    apiConfig   :: APIConfig
}

derive instance newtypeHouseConfig :: Newtype HouseConfig _
instance defaultHouseConfig :: Default HouseConfig where
    def = HouseConfig {
        leadId      : 0,
        houseId     : 0,
        roofPlates  : [],
        panels      : [],
        dataServer  : "",
        modeDyn     : step Showing empty,
        textureInfo : def,
        panelType   : step Standard empty,
        apiConfig   : def
    }

_roofPlates :: Lens' HouseConfig (Array RoofPlate)
_roofPlates = _Newtype <<< prop (SProxy :: SProxy "roofPlates")

_panels :: Lens' HouseConfig (Array Panel)
_panels = _Newtype <<< prop (SProxy :: SProxy "panels")

_dataServer :: Lens' HouseConfig String
_dataServer = _Newtype <<< prop (SProxy :: SProxy "dataServer")

_apiConfig :: Lens' HouseConfig APIConfig
_apiConfig = _Newtype <<< prop (SProxy :: SProxy "apiConfig")


newtype HouseEditor a = HouseEditor (ReaderT HouseConfig Effect a)

derive newtype instance functorHouseEditor     :: Functor HouseEditor
derive newtype instance applyHouseEditor       :: Apply HouseEditor
derive newtype instance applicativeHouseEditor :: Applicative HouseEditor
derive newtype instance bindHouseEditor        :: Bind HouseEditor
derive newtype instance monadHouseEditor       :: Monad HouseEditor
derive newtype instance monadEffectHouseEditor :: MonadEffect HouseEditor
derive newtype instance monadAskHouseEditor    :: MonadAsk HouseConfig HouseEditor
derive newtype instance monadReaderHouseEditor :: MonadReader HouseConfig HouseEditor

runHouseEditor :: forall a. HouseEditor a -> HouseConfig -> Effect a
runHouseEditor (HouseEditor e) = runReaderT e

performEditorEvent :: forall a. Event (HouseEditor a) -> HouseEditor (Event a)
performEditorEvent e = do
    cfg <- ask
    pure $ makeEvent \k -> subscribe e (\v -> runHouseEditor v cfg >>= k)

performEditorDyn :: forall a. Dynamic (HouseEditor a) -> HouseEditor (Dynamic a)
performEditorDyn d = do
    cfg <- ask

    curV <- liftEffect $ current d
    def <- liftEffect $ runHouseEditor curV cfg
    let evt = makeEvent \k -> subscribe (dynEvent d) \v -> runHouseEditor v cfg >>= k
    pure $ step def evt

runAPIInEditor :: forall a. API a -> HouseEditor a
runAPIInEditor api = ask >>= view _apiConfig >>> runAPI api >>> liftEffect