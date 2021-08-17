module API.Roofplate (loadRoofplates, buildRoofplates) where

import Prelude

import API (API, callAPI')
import Data.Argonaut.Decode (class DecodeJson)
import Data.Argonaut.Decode.Generic (genericDecodeJsonWith)
import Data.Argonaut.Types.Generic (defaultEncoding)
import Data.Generic.Rep (class Generic)
import Data.HTTP.Method (Method(..))
import FRP.Event (Event)
import Model.Roof.RoofPlate (RoofEdited, RoofPlate)

newtype RoofPlatesResult = RoofPlatesResult {
    roofplates :: Array RoofPlate
}

derive instance Generic RoofPlatesResult _
instance DecodeJson RoofPlatesResult where
    decodeJson = genericDecodeJsonWith (defaultEncoding { unwrapSingleArguments = true })

loadRoofplates :: Int -> API (Event (Array RoofPlate))
loadRoofplates i = map f <$> callAPI' GET url {}
    where url = "/v1/leads/" <> show i <> "/roofplates"
          f (RoofPlatesResult r) = r.roofplates


buildRoofplates :: Int -> Array RoofEdited -> API (Event Unit)
buildRoofplates houseId roofs = callAPI' POST ("/v1/houses/" <> show houseId <> "/lead/roofplates") roofs
