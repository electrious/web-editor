module API.Error where

import Util (ffi)

foreign import data ErrorPB :: Type

getCode :: ErrorPB -> Int
getCode = ffi ["e"] "e.code"

getMessage :: ErrorPB -> String
getMessage = ffi ["e"] "e.message"
