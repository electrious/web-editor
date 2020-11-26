module Rendering.Renderable where

import Prelude

import Control.Monad.Reader (class MonadAsk, class MonadReader, ReaderT, ask, runReaderT, withReaderT)
import Effect (Effect)
import Effect.Class (class MonadEffect)
import FRP.Event (Event)
import FRP.Event.Extra (performEvent)
import Math.Angle (Angle)
import Three.Core.Object3D (class IsObject3D)

newtype RenderingM e a = RenderingM (ReaderT e Effect a)

derive newtype instance functorRenderingM     :: Functor (RenderingM e)
derive newtype instance applyRenderingM       :: Apply (RenderingM e)
derive newtype instance applicativeRenderingM :: Applicative (RenderingM e)
derive newtype instance bindRenderingM        :: Bind (RenderingM e)
derive newtype instance monadRenderingM       :: Monad (RenderingM e)
derive newtype instance monadEffectRenderingM :: MonadEffect (RenderingM e)
derive newtype instance monadAskRenderingM    :: MonadAsk e (RenderingM e)
derive newtype instance monadReaderRenderingM :: MonadReader e (RenderingM e)

-- | run a RenderingM action in a specified context
runRenderingM :: forall a e. RenderingM e a -> e -> Effect a
runRenderingM (RenderingM r) = runReaderT r

-- | run a RenderingM action in a modified rendering context
withRenderContext :: forall a e g. (g -> e) -> RenderingM e a -> RenderingM g a
withRenderContext c (RenderingM r) = RenderingM $ withReaderT c r

-- perform RenderingM actions in an Event
performRenderingMEvt :: forall a e. Event (RenderingM e a) -> RenderingM e (Event a)
performRenderingMEvt e = do
    cfg <- ask
    pure $ performEvent $ (flip runRenderingM cfg) <$> e

class IsObject3D b <= Renderable e a b where
    render :: a -> RenderingM e b


class IsObject3D b <= RenderableWithSlope e a b where
    renderWithSlope :: Angle -> a -> RenderingM e b
