module UI.Bridge where

import Prelude

import Control.Monad.Cleanup (class MonadCleanup)
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import FRP.Dynamic (Dynamic, current, step, subscribeDyn)
import FRP.Event (Event, create, subscribe)
import Specular.FRP as S

toUIEvent :: forall a. Event a -> Effect (S.Event a)
toUIEvent e = do
    { event: ne, fire: f } <- S.newEvent
    _ <- subscribe e f
    pure ne


fromUIEvent :: forall a m. MonadEffect m => MonadCleanup m => S.Event a -> m (Event a)
fromUIEvent e = do
    { event: ne, push: f } <- liftEffect create
    S.subscribeEvent_ f e
    pure ne

toUIDyn :: forall a. Dynamic a -> Effect (S.Dynamic a)
toUIDyn d = do
    cur <- current d
    { dynamic: nd, set: f } <- S.newDynamic cur
    _ <- subscribeDyn d f
    pure nd

fromUIDyn :: forall a m. MonadEffect m => MonadCleanup m => S.Dynamic a -> m (Dynamic a)
fromUIDyn d = do
    cur <- liftEffect $ S.readBehavior $ S.current d
    e <- fromUIEvent $ S.changed d
    pure $ step cur e
