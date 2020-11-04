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
import Editor.Common.Lenses (_apiConfig)
import Editor.EditorMode (EditorMode(..))
import Editor.PanelNode (PanelOpacity)
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import FRP.Dynamic (Dynamic, step)
import FRP.Event (Event, makeEvent, subscribe)
import Model.Hardware.PanelTextureInfo (PanelTextureInfo)
import Model.Hardware.PanelType (PanelType(..))
import Model.Roof.Panel (Alignment, Orientation, Panel)
import Model.Roof.RoofPlate (RoofPlate)

newtype ArrayEditParam = ArrayEditParam {
    alignment   :: Event Alignment,
    orientation :: Event Orientation,
    opacity     :: Event PanelOpacity,
    heatmap     :: Event Boolean
}

derive instance newtypeArrayEditParam :: Newtype ArrayEditParam _

instance defaultArrayEditParam :: Default ArrayEditParam where
    def = ArrayEditParam {
        alignment   : empty,
        orientation : empty,
        opacity     : empty,
        heatmap     : empty
    }

_heatmap :: forall t a r. Newtype t { heatmap :: a | r } => Lens' t a
_heatmap = _Newtype <<< prop (SProxy :: SProxy "heatmap")

newtype HouseConfig = HouseConfig {
    leadId          :: Int,
    houseId         :: Int,
    roofplates      :: Array RoofPlate,
    panels          :: Array Panel,
    dataServer      :: String,
    modeDyn         :: Dynamic EditorMode,
    textureInfo     :: PanelTextureInfo,
    rotBtnTexture   :: String,
    heatmapTexture  :: String,
    panelType       :: Dynamic PanelType,
    apiConfig       :: APIConfig,
    screenshotDelay :: Int,

    arrayEditParam  :: ArrayEditParam
}

derive instance newtypeHouseConfig :: Newtype HouseConfig _
instance defaultHouseConfig :: Default HouseConfig where
    def = HouseConfig {
        leadId          : 0,
        houseId         : 0,
        roofplates      : [],
        panels          : [],
        dataServer      : "",
        modeDyn         : step Showing empty,
        textureInfo     : def,
        rotBtnTexture   : "",
        heatmapTexture  : "",
        panelType       : step Standard empty,
        apiConfig       : def,
        screenshotDelay : 100,

        arrayEditParam  : def
    }

_roofplates :: forall t a r. Newtype t { roofplates :: a | r } => Lens' t a
_roofplates = _Newtype <<< prop (SProxy :: SProxy "roofplates")

_dataServer :: Lens' HouseConfig String
_dataServer = _Newtype <<< prop (SProxy :: SProxy "dataServer")

_rotBtnTexture :: forall t a r. Newtype t { rotBtnTexture :: a | r } => Lens' t a
_rotBtnTexture = _Newtype <<< prop (SProxy :: SProxy "rotBtnTexture")

_heatmapTexture :: forall t a r. Newtype t { heatmapTexture :: a | r } => Lens' t a
_heatmapTexture = _Newtype <<< prop (SProxy :: SProxy "heatmapTexture")

_screenshotDelay :: forall t a r. Newtype t { screenshotDelay :: a | r } => Lens' t a
_screenshotDelay = _Newtype <<< prop (SProxy :: SProxy "screenshotDelay")

_arrayEditParam :: forall t a r. Newtype t { arrayEditParam :: a | r } => Lens' t a
_arrayEditParam = _Newtype <<< prop (SProxy :: SProxy "arrayEditParam")

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

runAPIInEditor :: forall a. API a -> HouseEditor a
runAPIInEditor api = ask >>= view _apiConfig >>> runAPI api >>> liftEffect
