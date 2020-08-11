module Rendering.Renderable where

import Prelude

import Control.Monad.Reader (class MonadAsk, class MonadReader, ReaderT, runReaderT)
import Data.Lens (Lens')
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy(..))
import Effect (Effect)
import Effect.Class (class MonadEffect)
import Math.Angle (Angle)
import Three.Core.Object3D (class IsObject3D)
import Three.Loader.TextureLoader (Texture)

newtype RendererConfig = RendererConfig {
    rotateButtonTexture :: Texture
}

derive instance newtypeRendererConfig :: Newtype RendererConfig _

_rotateButtonTexture :: forall t a r. Newtype t { rotateButtonTexture :: a | r } => Lens' t a
_rotateButtonTexture = _Newtype <<< prop (SProxy :: SProxy "rotateButtonTexture")


newtype RenderingM a = RenderingM (ReaderT RendererConfig Effect a)

derive newtype instance functorRenderingM     :: Functor RenderingM
derive newtype instance applyRenderingM       :: Apply RenderingM
derive newtype instance applicativeRenderingM :: Applicative RenderingM
derive newtype instance bindRenderingM        :: Bind RenderingM
derive newtype instance monadRenderingM       :: Monad RenderingM
derive newtype instance monadEffectRenderingM :: MonadEffect RenderingM
derive newtype instance monadAskRenderingM    :: MonadAsk RendererConfig RenderingM
derive newtype instance monadReaderRenderingM :: MonadReader RendererConfig RenderingM

runRenderingM :: forall a. RenderingM a -> RendererConfig -> Effect a
runRenderingM (RenderingM r) = runReaderT r


class IsObject3D b <= Renderable a b where
    render :: a -> RenderingM b


class IsObject3D b <= RenderableWithSlope a b where
    renderWithSlope :: Angle -> a -> RenderingM b
