module Util where

import Data.Foreign.EasyFFI (unsafeForeignFunction, unsafeForeignProcedure)

ffi :: forall a. Array String -> String -> a
ffi = unsafeForeignFunction
fpi :: forall a. Array String -> String -> a
fpi = unsafeForeignProcedure
