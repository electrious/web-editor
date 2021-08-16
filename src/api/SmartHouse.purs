module API.SmartHouse where

import Prelude

import API (API, callAPI', formAPI', performAPIEvent)
import Control.Alt ((<|>))
import Data.Filterable (filter)
import Data.Generic.Rep (class Generic)
import Data.Lens (Lens', view, (^.))
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Type.Proxy (Proxy(..))
import FRP.Event (Event, keepLatest)
import FRP.Event.Extra (delay, multicast)
import Foreign (Foreign)
import Foreign.Class (class Decode, class Encode)
import Foreign.Generic (defaultOptions, genericDecode)
import Model.ActiveMode (ActiveMode(..))
import Model.SmartHouse.House (JSHouses)
import OBJExporter (MeshFiles, _mtl, _obj)
import Web.File (File)


data SavingStep = NotSaving
                | UploadingFiles
                | CreatingHouse
                | WaitingForReady
                | Finished Int

derive instance genericSavingStep :: Generic SavingStep _
derive instance eqSavingStep :: Eq SavingStep
instance showSavingStep :: Show SavingStep where
    show NotSaving       = "Not saving"
    show UploadingFiles  = "Uploading mesh files for the new house..."
    show CreatingHouse   = "Elli is analyzing the new house data..."
    show WaitingForReady = "Elli is analyzing the new house data..."
    show (Finished _)    = "Finished creating the new house"

stepMode :: SavingStep -> ActiveMode
stepMode NotSaving    = Inactive
stepMode (Finished _) = Inactive
stepMode _            = Active

isFinished :: SavingStep -> Boolean
isFinished (Finished _) = true
isFinished _            = false

savedHouseId :: SavingStep -> Maybe Int
savedHouseId (Finished h) = Just h
savedHouseId _            = Nothing


newtype UploadReq = UploadReq {
    obj     :: File,
    mtl     :: File,
    texture :: File
    }

derive instance newtypeUploadReq :: Newtype UploadReq _

foreign import toFormData :: UploadReq -> Foreign

instance encodeUploadReq :: Encode UploadReq where
    encode = toFormData

mkUploadReq :: MeshFiles -> File -> UploadReq
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
uploadMeshFiles :: Int -> MeshFiles -> File -> API (Event APIResp)
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
fieldTrans "success" = "ready"
fieldTrans "houseId" = "house_id"
fieldTrans _         = ""


_success :: forall t a r. Newtype t { success :: a | r } => Lens' t a
_success = _Newtype <<< prop (Proxy :: Proxy "success")


succeeded :: ReadyAPIResp -> Boolean
succeeded = view _success

-- API to check if the house is aready or not
checkReady :: Int -> API (Event ReadyAPIResp)
checkReady lid = callAPI' POST ("/v1/lead/" <> show lid <> "/ready") {}

-- check if the 2d house is ready or not repeatedly with a 2 seconds sleep, until it's ready.
repeatCheckUntilReady :: Int -> API (Event ReadyAPIResp)
repeatCheckUntilReady lid = do
    re <- multicast <$> checkReady lid
    let se = filter succeeded re
        fe = filter (not <<< succeeded) re

    ee <- performAPIEvent $ const (repeatCheckUntilReady lid) <$> delay 2000 fe
    pure $ se <|> keepLatest ee
