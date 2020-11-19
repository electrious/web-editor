module Rendering.CacheRenderer where

import Prelude

import Control.Monad.State (class MonadState, StateT, lift)
import Data.Default (class Default)
import Data.Lens (Lens')
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.List (List(..))
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy(..))
import Effect.Class (class MonadEffect)
import Rendering.Renderable (class Renderable, RenderingM, render)
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


newtype CacheRenderingM c a = CacheRenderingM (StateT (Cache c) RenderingM a)

derive newtype instance functorCacheRenderingM     :: Functor (CacheRenderingM c)
derive newtype instance applyCacheRenderingM       :: Apply (CacheRenderingM c)
derive newtype instance applicativeRenderingM      :: Applicative (CacheRenderingM c)
derive newtype instance bindCacheRenderingM        :: Bind (CacheRenderingM c)
derive newtype instance monadCacheRenderingM       :: Monad (CacheRenderingM c)
derive newtype instance monadStateCacheRenderingM  :: MonadState (Cache c) (CacheRenderingM c)
derive newtype instance monadEffectCacheRenderingM :: MonadEffect (CacheRenderingM c)

liftRenderingM :: forall c a. RenderingM a -> CacheRenderingM c a
liftRenderingM r = CacheRenderingM $ lift r

renderWithCache :: forall p c cn. IsObject3D p => Renderable c cn => p -> c -> CacheRenderingM c cn
renderWithCache parent val = liftRenderingM $ render val
