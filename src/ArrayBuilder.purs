module Editor.ArrayBuilder where

import Prelude

import Control.Monad.Reader (class MonadAsk, class MonadReader, ReaderT, ask, lift, runReaderT)
import Control.Plus (empty)
import Data.Lens (Lens', view)
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy(..))
import Editor.Common.Lenses (_panelType, _textureInfo)
import Editor.HouseEditor (HouseEditor, _rotBtnTexture)
import Editor.UI.RotateButton (rotateBtnTexture)
import Effect.Class (class MonadEffect, liftEffect)
import FRP.Dynamic (Dynamic, step)
import Model.Hardware.PanelTextureInfo (PanelTextureInfo)
import Model.Hardware.PanelType (PanelType)
import Model.Racking.RackingType (RackingType)
import Model.Roof.ArrayConfig (ArrayConfig, arrayConfigForRack)
import Rendering.Renderable (RendererConfig(..), RenderingM, runRenderingM)

-- data used to provide env info for ArrayBuilder monad
newtype ArrayBuilderEnv = ArrayBuilderEnv {
    arrayConfig :: Dynamic ArrayConfig,
    textureInfo :: PanelTextureInfo,
    panelType   :: Dynamic PanelType
}

derive instance newtypeArrayBuilderEnv :: Newtype ArrayBuilderEnv _

_arrayConfig :: forall t a r. Newtype t { arrayConfig :: a | r } => Lens' t a
_arrayConfig = _Newtype <<< prop (SProxy :: SProxy "arrayConfig")

-- | ArrayBuilder is the monad to build arrays in
newtype ArrayBuilder a = ArrayBuilder (ReaderT ArrayBuilderEnv RenderingM a)

derive newtype instance functorArrayBuilder     :: Functor ArrayBuilder
derive newtype instance applyArrayBuilder       :: Apply ArrayBuilder
derive newtype instance applicativeArrayBuilder :: Applicative ArrayBuilder
derive newtype instance bindArrayBuilder        :: Bind ArrayBuilder
derive newtype instance monadArrayBuilder       :: Monad ArrayBuilder
derive newtype instance monadAskArrayBuilder    :: MonadAsk ArrayBuilderEnv ArrayBuilder
derive newtype instance monadReaderArrayBuilder :: MonadReader ArrayBuilderEnv ArrayBuilder
derive newtype instance monadEffectArrayBuilder :: MonadEffect ArrayBuilder

liftRenderingM :: forall a. RenderingM a -> ArrayBuilder a
liftRenderingM r = ArrayBuilder $ lift r

runArrayBuilder :: forall a. RackingType -> ArrayBuilder a -> HouseEditor a
runArrayBuilder rt (ArrayBuilder b) = do
    texture <- view _textureInfo   <$> ask
    btnText <- view _rotBtnTexture <$> ask
    pt      <- view _panelType     <$> ask
    let env = ArrayBuilderEnv {
                arrayConfig : step (arrayConfigForRack rt) empty,
                textureInfo : texture,
                panelType   : pt
             }
        rCfg = RendererConfig {
            rotateButtonTexture: rotateBtnTexture btnText
        }
    liftEffect $ runRenderingM (runReaderT b env) rCfg

getArrayConfig :: ArrayBuilder (Dynamic ArrayConfig)
getArrayConfig = view _arrayConfig <$> ask

getTextureInfo :: ArrayBuilder PanelTextureInfo
getTextureInfo = view _textureInfo <$> ask

getPanelType :: ArrayBuilder (Dynamic PanelType)
getPanelType = view _panelType <$> ask