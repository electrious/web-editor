module Editor.ArrayBuilder where

import Prelude

import Control.Monad.Reader (class MonadAsk, class MonadReader, ReaderT, ask)
import Data.Default (class Default)
import Data.Generic.Rep (class Generic)
import Data.Lens (Lens', view)
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy(..))
import Effect (Effect)
import Effect.Class (class MonadEffect)
import FRP.Event (Event)
import Model.Hardware.PanelType (PanelType)
import Model.Roof.ArrayConfig (ArrayConfig)


-- texture info needed to render panels
newtype PanelTextureInfo = PanelTextureInfo {
    standard   :: Maybe String,
    premium    :: Maybe String,
    standard72 :: Maybe String
}

derive instance newtypePanelTextureInfo :: Newtype PanelTextureInfo _
derive instance genericPanelTextureInfo :: Generic PanelTextureInfo _
instance defaultPanelTextureInfo :: Default PanelTextureInfo where
    def = PanelTextureInfo { standard   : Nothing,
                             premium    : Nothing,
                             standard72 : Nothing
                           }

_standard :: Lens' PanelTextureInfo (Maybe String)
_standard = _Newtype <<< prop (SProxy :: SProxy "standard")

_premium :: Lens' PanelTextureInfo (Maybe String)
_premium = _Newtype <<< prop (SProxy :: SProxy "premium")

_standard72 :: Lens' PanelTextureInfo (Maybe String)
_standard72 = _Newtype <<< prop (SProxy :: SProxy "standard72")

-- data used to provide env info for ArrayBuilder monad
newtype ArrayBuilderEnv = ArrayBuilderEnv {
    arrayConfig :: ArrayConfig,
    textureInfo :: PanelTextureInfo,
    panelType   :: Event PanelType
}

derive instance newtypeArrayBuilderEnv :: Newtype ArrayBuilderEnv _

_arrayConfig :: Lens' ArrayBuilderEnv ArrayConfig
_arrayConfig = _Newtype <<< prop (SProxy :: SProxy "arrayConfig")

_textureInfo :: Lens' ArrayBuilderEnv PanelTextureInfo
_textureInfo = _Newtype <<< prop (SProxy :: SProxy "textureInfo")

_panelType :: Lens' ArrayBuilderEnv (Event PanelType)
_panelType = _Newtype <<< prop (SProxy :: SProxy "panelType")

-- | ArrayBuilder is the monad to build arrays in
newtype ArrayBuilder a = ArrayBuilder (ReaderT ArrayBuilderEnv Effect a)

derive newtype instance functorArrayBuilder :: Functor ArrayBuilder
derive newtype instance applyArrayBuilder :: Apply ArrayBuilder
derive newtype instance applicativeArrayBuilder :: Applicative ArrayBuilder
derive newtype instance bindArrayBuilder :: Bind ArrayBuilder
derive newtype instance monadArrayBuilder :: Monad ArrayBuilder
derive newtype instance monadAskArrayBuilder :: MonadAsk ArrayBuilderEnv ArrayBuilder
derive newtype instance monadReaderArrayBuilder :: MonadReader ArrayBuilderEnv ArrayBuilder
derive newtype instance monadEffectArrayBuilder :: MonadEffect ArrayBuilder

getArrayConfig :: ArrayBuilder ArrayConfig
getArrayConfig = view _arrayConfig <$> ask

getTextureInfo :: ArrayBuilder PanelTextureInfo
getTextureInfo = view _textureInfo <$> ask

getPanelType :: ArrayBuilder (Event PanelType)
getPanelType = view _panelType <$> ask