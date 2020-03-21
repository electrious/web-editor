module Util where

import Data.Foreign.EasyFFI
import Prelude

import Data.DateTime.Instant (Instant)
import Data.Filterable (filter)
import Data.Maybe (Maybe(..), fromJust, isJust)
import Effect.Now (now)
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

unwrap :: forall a. Event (Maybe a) -> Event a
unwrap e = unsafePartial (fromJust <$> filter isJust e)

-- | skip first n occurrences of the event
skip :: forall a. Int -> Event a -> Event a
skip n evt = gate skipped evt
    where c = step 0 (count evt)
          skipped = ((>) n) <$> c

distinct :: forall a. Eq a => Event a -> Event a
distinct evt = getNow <$> filter isDiff (withLast evt)
    where isDiff { last, now } = last == Just now
          getNow { now } = now
