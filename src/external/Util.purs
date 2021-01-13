module Util where

import Prelude

import Control.Alt ((<|>))
import Control.Plus (empty)
import Data.Foldable (class Foldable, foldl)
import Effect (Effect)
import FRP.Event (Event, create, makeEvent, subscribe)

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
