module Rendering.CacheRenderer (class ReRenderable, updateNode, CacheRenderingM,
    runCacheRenderingM, render, rerenderAll) where

import Prelude hiding (add)

import Control.Monad.Reader (class MonadAsk, class MonadReader, ReaderT, ask, runReaderT)
import Control.Monad.State (class MonadState, StateT, evalStateT, get, lift, modify, put)
import Data.Default (class Default)
import Data.Foldable (class Foldable, traverse_)
import Data.Lens (Lens', (^.), (.~), (%~))
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.List (List(..), drop, head, length, tail, take)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy(..))
import Editor.Disposable (class Disposable, dispose)
import Effect.Class (class MonadEffect, liftEffect)
import Rendering.Renderable (class Renderable, RenderingM)
import Rendering.Renderable as R
import Three.Core.Object3D (class IsObject3D, add, remove)


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

runCacheRenderingM :: forall p c a. CacheRenderingM p c a -> p -> RenderingM a
runCacheRenderingM (CacheRenderingM m) p = runReaderT (evalStateT m (Cache { rendering: Nil, idle : Nil })) p

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

-- clean up idle nodes to reduce memory usage
cleanUpIdle :: forall p cn. Disposable cn => CacheRenderingM p cn Unit
cleanUpIdle = do
    s <- get

    let nr  = length $ s ^. _rendering
        ni  = length $ s ^. _idle
        nr2 = nr / 2
    when (ni > nr2) do
        let is = s ^. _idle
            i1 = take nr2 is
            i2 = drop nr2 is
        put $ s # _idle .~ i1
        liftEffect $ traverse_ dispose i2

-- | render a new value in CacheRenderingM
render :: forall p c cn. IsObject3D p => ReRenderable c cn => c -> CacheRenderingM p cn Unit
render val = do
    p <- ask
    n <- render' val
    liftEffect $ add n p
    void $ modify (_rendering %~ Cons n)
    pure unit

-- | rerenderAll will delete all rendered nodes and render new values in the list given
rerenderAll :: forall p c cn f. IsObject3D p => ReRenderable c cn => Disposable cn => Foldable f => f c -> CacheRenderingM p cn Unit
rerenderAll vs = do
    p  <- ask
    st <- get
    -- remove rendered nodes and move them to the idle list
    let rns = st ^. _rendering
    liftEffect $ traverse_ (flip remove p) rns
    put $ st # _rendering .~ Nil
             # _idle      %~ append rns
    traverse_ render vs
    cleanUpIdle
