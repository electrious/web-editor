module Util where

import Data.Foreign.EasyFFI
import Prelude

import Data.DateTime.Instant (Instant)
import Data.Filterable (filter)
import Data.Maybe (Maybe(..), fromJust, isJust)
import Effect (Effect)
import Effect.Now (now)
import Effect.Ref as Ref
import Effect.Timer (clearTimeout, setTimeout)
import FRP.Behavior (gate, step)
import FRP.Event (Event, count, makeEvent, subscribe, withLast)
import Partial.Unsafe (unsafePartial)

ffi :: forall a. Array String -> String -> a
ffi = unsafeForeignFunction
fpi :: forall a. Array String -> String -> a
fpi = unsafeForeignProcedure


-- | create an Event that will fire in n milliseconds
after :: Int -> Event Instant
after n = makeEvent \k -> do
    id <- setTimeout n do
        time <- now
        k time
    pure (clearTimeout id)

-- | delay the input event by n milliseconds
-- TODO: add code to clear timeouts when cancelled
delay :: forall a. Int -> Event a -> Event a
delay n evt = makeEvent \k -> do
    subscribe evt \v -> do
        setTimeout n (k v)

-- | skip first n occurrences of the event
skip :: forall a. Int -> Event a -> Event a
skip n evt = gate skipped evt
    where c = step 0 (count evt)
          skipped = ((>) n) <$> c

distinct :: forall a. Eq a => Event a -> Event a
distinct evt = getNow <$> filter isDiff (withLast evt)
    where isDiff { last, now } = last == Just now
          getNow { now } = now

-- | Perform events with actions inside.
performEvent :: forall a. Event (Effect a) -> Event a
performEvent evt = makeEvent \k -> do
    canceller <- subscribe evt \act -> act >>= k
    pure canceller

-- | fold events with Effect actions 
foldEffect :: forall a b. (a -> b -> Effect b) -> Event a -> b -> Event b
foldEffect act e b = makeEvent \k -> do
    result <- Ref.new b
    subscribe e \a -> do
        curB <- Ref.read result
        newB <- act a curB
        Ref.write newB result
        k newB
