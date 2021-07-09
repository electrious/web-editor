module Editor.ArrayBuilder where

import Prelude

import API (APIConfig)
import Control.Monad.Reader (class MonadAsk, class MonadReader, ReaderT, ask, lift, runReaderT)
import Data.Lens (Lens', view, (^.))
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Newtype (class Newtype)
import Type.Proxy (Proxy(..))
import Editor.Common.Lenses (_apiConfig, _modeDyn, _panelType, _textureInfo)
import Editor.EditorMode (EditorMode)
import Editor.HouseEditor (HouseEditor, _heatmapTexture, _rotBtnTexture)
import Effect.Class (class MonadEffect, liftEffect)
import FRP.Dynamic (Dynamic)
import Model.Hardware.PanelTextureInfo (PanelTextureInfo)
import Model.Hardware.PanelType (PanelType)
import Model.Racking.RackingType (RackingType)
import Model.Roof.ArrayConfig (ArrayConfig, arrayConfigForRack)
import Rendering.Renderable (RenderingM, runRenderingM)
import Rendering.TextureLoader (loadMaterialFromUrl)
import Three.Core.Material (MeshBasicMaterial, doubleSide, setSide)

-- data used to provide env info for ArrayBuilder monad
newtype ArrayBuilderEnv = ArrayBuilderEnv {
    arrayConfig :: Dynamic ArrayConfig,
    textureInfo :: PanelTextureInfo,
    panelType   :: Dynamic PanelType,
    editorMode  :: Dynamic EditorMode,
    apiConfig   :: APIConfig
}

derive instance newtypeArrayBuilderEnv :: Newtype ArrayBuilderEnv _

_arrayConfig :: forall t a r. Newtype t { arrayConfig :: a | r } => Lens' t a
_arrayConfig = _Newtype <<< prop (Proxy :: Proxy "arrayConfig")

_editorMode :: forall t a r. Newtype t { editorMode :: a | r } => Lens' t a
_editorMode = _Newtype <<< prop (Proxy :: Proxy "editorMode")

newtype RendererConfig = RendererConfig {
    heatmapMaterial      :: MeshBasicMaterial,
    rotateButtonMaterial :: MeshBasicMaterial
}

derive instance newtypeRendererConfig :: Newtype RendererConfig _

_heatmapMaterial :: forall t a r. Newtype t { heatmapMaterial :: a | r } => Lens' t a
_heatmapMaterial = _Newtype <<< prop (Proxy :: Proxy "heatmapMaterial")

_rotateButtonMaterial :: forall t a r. Newtype t { rotateButtonMaterial :: a | r } => Lens' t a
_rotateButtonMaterial = _Newtype <<< prop (Proxy :: Proxy "rotateButtonMaterial")

-- | ArrayBuilder is the monad to build arrays in
newtype ArrayBuilder a = ArrayBuilder (ReaderT ArrayBuilderEnv (RenderingM RendererConfig) a)

derive newtype instance functorArrayBuilder     :: Functor ArrayBuilder
derive newtype instance applyArrayBuilder       :: Apply ArrayBuilder
derive newtype instance applicativeArrayBuilder :: Applicative ArrayBuilder
derive newtype instance bindArrayBuilder        :: Bind ArrayBuilder
derive newtype instance monadArrayBuilder       :: Monad ArrayBuilder
derive newtype instance monadAskArrayBuilder    :: MonadAsk ArrayBuilderEnv ArrayBuilder
derive newtype instance monadReaderArrayBuilder :: MonadReader ArrayBuilderEnv ArrayBuilder
derive newtype instance monadEffectArrayBuilder :: MonadEffect ArrayBuilder

liftRenderingM :: forall a. RenderingM RendererConfig a -> ArrayBuilder a
liftRenderingM r = ArrayBuilder $ lift r

runArrayBuilder :: forall a. Dynamic RackingType -> ArrayBuilder a -> HouseEditor a
runArrayBuilder rtDyn (ArrayBuilder b) = do
    cfg <- ask
    let texture = cfg ^. _textureInfo
        hmText  = cfg ^. _heatmapTexture
        btnText = cfg ^. _rotBtnTexture
        pt      = cfg ^. _panelType
        modeDyn = cfg ^. _modeDyn
        apiCfg  = cfg ^. _apiConfig
    
    let env = ArrayBuilderEnv {
                arrayConfig : arrayConfigForRack <$> rtDyn,
                textureInfo : texture,
                panelType   : pt,
                editorMode  : modeDyn,
                apiConfig   : apiCfg
             }
        hmMat = loadMaterialFromUrl hmText
    
    -- make heatmap material double side rendering
    liftEffect $ setSide doubleSide hmMat

    let rCfg = RendererConfig {
            heatmapMaterial      : hmMat,
            rotateButtonMaterial : loadMaterialFromUrl btnText
        }
    liftEffect $ runRenderingM (runReaderT b env) rCfg

getArrayConfig :: ArrayBuilder (Dynamic ArrayConfig)
getArrayConfig = view _arrayConfig <$> ask

getTextureInfo :: ArrayBuilder PanelTextureInfo
getTextureInfo = view _textureInfo <$> ask

getPanelType :: ArrayBuilder (Dynamic PanelType)
getPanelType = view _panelType <$> ask
