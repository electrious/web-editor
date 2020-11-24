module Rendering.CacheRenderer where

import Prelude hiding (add)

import Control.Monad.Reader (class MonadAsk, class MonadReader, ReaderT)
import Control.Monad.State (class MonadState, StateT, get, lift, put)
import Data.Default (class Default)
import Data.Lens (Lens', (^.), (%~))
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.List (List(..), head, tail)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy(..))
import Effect.Class (class MonadEffect)
import Rendering.Renderable (class Renderable, RenderingM)
import Rendering.Renderable as R
import Three.Core.Object3D (class IsObject3D)


newtype Cache a = Cache {
    rendering :: List a,
    idle      :: List a
}
derive instance newtypeCache :: Newtype (Cache a) _
instance defaultCache :: Default (Cache a) where
    def = Cache { rendering : Nil, idle : Nil }

_rendering :: forall t a r. Newtype t { rendering :: a | r } => Lens' t a
_rendering = _Newtype <<< prop (SProxy :: SProxy "rendering")

_idle :: forall t a r. Newtype t { idle :: a | r } => Lens' t a
_idle = _Newtype <<< prop (SProxy :: SProxy "idle")


-- | ReRenderable will be ab able to update rendered node with new value
class Renderable v n <= ReRenderable v n where
    updateNode :: n -> v -> RenderingM Unit

newtype CacheRenderingM p c a = CacheRenderingM (StateT (Cache c) (ReaderT p RenderingM) a)

derive newtype instance functorCacheRenderingM     :: Functor (CacheRenderingM p c)
derive newtype instance applyCacheRenderingM       :: Apply (CacheRenderingM p c)
derive newtype instance applicativeRenderingM      :: Applicative (CacheRenderingM p c)
derive newtype instance bindCacheRenderingM        :: Bind (CacheRenderingM p c)
derive newtype instance monadCacheRenderingM       :: Monad (CacheRenderingM p c)
derive newtype instance monadStateCacheRenderingM  :: MonadState (Cache c) (CacheRenderingM p c)
derive newtype instance monadAskCacheRenderingM    :: MonadAsk p (CacheRenderingM p c)
derive newtype instance monadReaderCacheRenderingM :: MonadReader p (CacheRenderingM p c)
derive newtype instance monadEffectCacheRenderingM :: MonadEffect (CacheRenderingM p c)

liftRenderingM :: forall p c a. RenderingM a -> CacheRenderingM p c a
liftRenderingM r = CacheRenderingM $ lift $ lift r

-- render a value to a new or idle node, internal use only
render' :: forall p c cn. IsObject3D p => ReRenderable c cn => c -> CacheRenderingM p cn cn
render' val = do
    c <- get
    let idle = c ^. _idle
    case head idle of
        Nothing -> do
            n <- CacheRenderingM $ lift $ lift $ R.render val
            pure n
        Just n  -> do
            liftRenderingM $ updateNode n val
            let nc = c # _idle %~ tail >>> fromMaybe Nil
            put nc
            pure n

render :: forall p c cn. IsObject3D p => ReRenderable c cn => c -> CacheRenderingM p cn Unit
render val = do
    