module Util where

import Prelude

import Control.Alt ((<|>))
import Control.Plus (empty)
import Data.Foldable (class Foldable, foldl)
import Data.FunctorWithIndex (class FunctorWithIndex, mapWithIndex)
import Effect (Effect)
import FRP.Dynamic (Dynamic, latestEvt)
import FRP.Event (Event, create, makeEvent, subscribe)
import FRP.Event.Extra (anyEvt)

-- | create a fix point with an Effect action for Events
fixE :: forall i o. (Event i -> Effect { input :: Event i, output :: Event o }) -> Event o
fixE f = makeEvent \k -> do
    { event, push } <- create
    { input, output } <- f event
    d1 <- subscribe input push
    d2 <- subscribe output k
    pure (d1 *> d2)


-- | accumulate event value in a foldable list of values with the specified retreive function
foldEvtWith :: forall a b f. Foldable f => Functor f => (a -> Event b) -> f a -> Event b
foldEvtWith f l = foldl (<|>) empty (f <$> l)

latestAnyEvt :: forall a f. Foldable f => Dynamic (f (Event a)) -> Event a
latestAnyEvt = latestEvt <<< map anyEvt

latestAnyEvtWith :: forall a b f. Functor f => Foldable f => (a -> Event b) -> Dynamic (f a) -> Event b
latestAnyEvtWith f = latestEvt <<< map (anyEvt <<< map f)

latestAnyEvtWithIdx :: forall a b f i. FunctorWithIndex i f => Foldable f => (i -> a -> Event b) -> Dynamic (f a) -> Event b
latestAnyEvtWithIdx f = latestEvt <<< map (anyEvt <<< mapWithIndex f)
