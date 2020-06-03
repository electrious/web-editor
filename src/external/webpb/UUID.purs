module WebPB.UUID where

import Prelude

import Effect (Effect)
import Util (ffi, fpi)

foreign import data PBUUID :: Type

foreign import mkPBUUID :: Effect PBUUID

getUUIDString :: PBUUID -> String
getUUIDString = ffi ["u"] "u.getUuid()"

setUUIDString :: String -> PBUUID -> Effect Unit
setUUIDString = fpi ["v", "u", ""] "u.setUuid(v)"
