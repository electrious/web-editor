module API.Error where

foreign import data ErrorPB :: Type

foreign import getCode :: ErrorPB -> Int
foreign import getMessage :: ErrorPB -> String
