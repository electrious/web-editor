module API.Roofplate (getRoofplates) where

import Prelude

import API (API, callAPI')
import Axios.Types (Method(..))
import Data.Generic.Rep (class Generic)
import FRP.Event (Event)
import Foreign.Generic (class Decode, defaultOptions, genericDecode)
import Model.Roof.RoofPlate (RoofPlate)

newtype RoofPlatesResult = RoofPlatesResult {
    roofplates :: Array RoofPlate
}

derive instance genericRoofPlatesResult :: Generic RoofPlatesResult _
instance decodeRoofPlatesResult :: Decode RoofPlatesResult where
    decode = genericDecode (defaultOptions { unwrapSingleConstructors = true })

getRoofplates :: Int -> API (Event (Array RoofPlate))
getRoofplates i = map f <$> callAPI' GET url {}
    where url = "/leads/" <> show i <> "/roofplates"
          f (RoofPlatesResult r) = r.roofplates
