module API.Racking where

import Prelude

import API (API, callAPI)
import Axios (Method(..))
import Data.Generic.Rep (class Generic)
import FRP.Event (Event)
import Foreign.Generic (class Decode, F, defaultOptions, genericDecode)
import Model.Racking.OldRackingSystem (OldRackingSystem)

newtype RackingResp = RackingResp {
    racking :: OldRackingSystem
}

derive instance genericRackingResp :: Generic RackingResp _
instance decodeRackingResp :: Decode RackingResp where
    decode = genericDecode (defaultOptions { unwrapSingleConstructors = true })


loadRacking :: Int -> API (Event (F OldRackingSystem))
loadRacking leadId = map (map getRacking) <$> callAPI GET ("/leads/" <> show leadId <> "/racking") {}
    where getRacking (RackingResp r) = r.racking
