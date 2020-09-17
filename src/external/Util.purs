module Util where

import Prelude

import Data.Foreign.EasyFFI (unsafeForeignFunction, unsafeForeignProcedure)
import Effect (Effect)
import FRP.Event (Event, create, makeEvent, subscribe)

ffi :: forall a. Array String -> String -> a
ffi = unsafeForeignFunction
fpi :: forall a. Array String -> String -> a
fpi = unsafeForeignProcedure

-- | create a fix point with an Effect action for Events
fixE :: forall i o. (Event i -> Effect { input :: Event i, output :: Event o }) -> Event o
fixE f = makeEvent \k -> do
    { event, push } <- create
    { input, output } <- f event
    d1 <- subscribe input push
    d2 <- subscribe output k
    pure (d1 *> d2)
