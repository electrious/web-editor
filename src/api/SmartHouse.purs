module API.SmartHouse where

import Prelude

import API (API, callAPI', formAPI', performAPIEvent)
import Axios.Types (Method(..))
import Data.Generic.Rep (class Generic)
import Data.Lens (Lens', (^.))
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy(..))
import Effect.Class (liftEffect)
import FRP.Event (Event)
import Foreign (Foreign)
import Foreign.Class (class Decode, class Encode)
import Foreign.Generic (defaultOptions, genericDecode)
import Model.SmartHouse.House (JSHouses)
import OBJExporter (MeshFiles, _mtl, _obj)


newtype UploadReq = UploadReq {
    obj     :: String,
    mtl     :: String,
    texture :: String
    }

derive instance newtypeUploadReq :: Newtype UploadReq _

foreign import toFormData :: UploadReq -> Foreign

instance encodeUploadReq :: Encode UploadReq where
    encode = toFormData

mkUploadReq :: MeshFiles -> String -> UploadReq
mkUploadReq m t = UploadReq {
    obj     : m ^. _obj,
    mtl     : m ^. _mtl,
    texture : t
    }

newtype APIResp = APIResp {
    success :: Boolean
    }

derive instance newtypeAPIResp :: Newtype APIResp _
derive instance genericAPIResp :: Generic APIResp _
instance decodeAPIResp :: Decode APIResp where
    decode = genericDecode (defaultOptions { unwrapSingleConstructors = true })

-- API to upload obj/mtl/texture files
uploadMeshFiles :: Int -> MeshFiles -> String -> API (Event APIResp)
uploadMeshFiles lid m t = formAPI' POST ("/v1/lead/" <> show lid <> "/scene/upload") $ mkUploadReq
 m t

-- API to create manual house/roof data
createManual :: Int -> JSHouses -> API (Event APIResp)
createManual lid = callAPI' POST ("/v1/lead/" <> show lid <> "/create-manual")


newtype ReadyAPIResp = ReadyAPIResp {
    success :: Boolean,
    houseId :: Int
    }

derive instance newtypeReadyAPIResp :: Newtype ReadyAPIResp _
derive instance genericReadyAPIResp :: Generic ReadyAPIResp _
instance decodeReadyAPIResp :: Decode ReadyAPIResp where
    decode = genericDecode (defaultOptions { unwrapSingleConstructors = true,
                                             fieldTransform = fieldTrans
                                           })

fieldTrans :: String -> String
fieldTrans "success" = "success"
fieldTrans "houseId" = "house_id"
fieldTrans _         = ""


_success :: forall t a r. Newtype t { sucess :: a | r } => Lens' t a
_success = _Newtype <<< prop (SProxy :: SProxy "sucess")


-- API to check if the house is aready or not
checkReady :: Int -> API (Event ReadyAPIResp)
checkReady lid = callAPI' POST ("/v1/lead/" <> show lid <> "/ready") {}

-- check if the 2d house is ready or not repeatedly with a 2 seconds sleep, until it's ready.
repeatCheckUntilReady :: Int -> API (Event ReadyAPIResp)
repeatCheckUntilReady lid = f
    where f = checkReady lid >>> processEvt
          processEvt e = performAPIEvent $ g <$> e
          g resp = if resp ^. _success
                   then pure resp
                   else checkReady lid
