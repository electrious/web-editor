module API.Roofplate (loadRoofplates, buildRoofplates) where

import Prelude

import API (API, callAPI', callAPI_')
import Data.Argonaut.Decode (class DecodeJson, decodeJson, (.:))
import Data.Generic.Rep (class Generic)
import Data.HTTP.Method (Method(..))
import FRP.Event (Event)
import Model.Roof.RoofPlate (RoofEdited, RoofPlate)

newtype RoofPlatesResult = RoofPlatesResult {
    roofplates :: Array RoofPlate
}

derive instance Generic RoofPlatesResult _
instance DecodeJson RoofPlatesResult where
    decodeJson = decodeJson >=> f
        where f o = mkRoofPlatesResult <$> o .: "roofplates"
    
mkRoofPlatesResult :: Array RoofPlate -> RoofPlatesResult
mkRoofPlatesResult roofplates = RoofPlatesResult { roofplates }

loadRoofplates :: Int -> API (Event (Array RoofPlate))
loadRoofplates i = map f <$> callAPI' GET url {}
    where url = "/v1/leads/" <> show i <> "/roofplates"
          f (RoofPlatesResult r) = r.roofplates


buildRoofplates :: Int -> Array RoofEdited -> API (Event Unit)
buildRoofplates houseId roofs = callAPI_' POST ("/v1/houses/" <> show houseId <> "/lead/roofplates") roofs
