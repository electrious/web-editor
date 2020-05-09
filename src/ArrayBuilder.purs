module Editor.ArrayBuilder where

import Prelude

import Control.Monad.Reader (class MonadAsk, class MonadReader, ReaderT, ask, runReaderT)
import Data.Lens (Lens', view)
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy(..))
import Editor.Common.Lenses (_panelType, _rackingType, _textureInfo)
import Editor.WebEditor (WebEditor)
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import FRP.Dynamic (Dynamic)
import Model.Hardware.PanelTextureInfo (PanelTextureInfo)
import Model.Hardware.PanelType (PanelType)
import Model.Roof.ArrayConfig (ArrayConfig, arrayConfigForRack)

-- data used to provide env info for ArrayBuilder monad
newtype ArrayBuilderEnv = ArrayBuilderEnv {
    arrayConfig :: Dynamic ArrayConfig,
    textureInfo :: PanelTextureInfo,
    panelType   :: Dynamic PanelType
}

derive instance newtypeArrayBuilderEnv :: Newtype ArrayBuilderEnv _

_arrayConfig :: Lens' ArrayBuilderEnv (Dynamic ArrayConfig)
_arrayConfig = _Newtype <<< prop (SProxy :: SProxy "arrayConfig")

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

runArrayBuilder :: forall a. ArrayBuilder a -> WebEditor a
runArrayBuilder (ArrayBuilder b) = do
    texture <- view _textureInfo <$> ask
    pt <- view _panelType <$> ask
    rt <- view _rackingType <$> ask
    liftEffect $ runReaderT b $ ArrayBuilderEnv {
        arrayConfig : arrayConfigForRack <$> rt,
        textureInfo : texture,
        panelType   : pt
    }

getArrayConfig :: ArrayBuilder (Dynamic ArrayConfig)
getArrayConfig = view _arrayConfig <$> ask

getTextureInfo :: ArrayBuilder PanelTextureInfo
getTextureInfo = view _textureInfo <$> ask

getPanelType :: ArrayBuilder (Dynamic PanelType)
getPanelType = view _panelType <$> ask