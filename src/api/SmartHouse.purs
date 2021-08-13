module API.SmartHouse where

import Prelude

import API (API, callAPI, formAPI, performAPIEvent)
import Axios.Types (Method(..))
import Control.Alt ((<|>))
import Data.Default (class Default)
import Data.Either (Either(..))
import Data.Filterable (filter)
import Data.Generic.Rep (class Generic)
import Data.Lens (Lens', (^.))
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import FRP.Event (Event, keepLatest)
import FRP.Event.Extra (delay, multicast)
import Foreign (Foreign, MultipleErrors)
import Foreign.Class (class Decode, class Encode)
import Foreign.Generic (defaultOptions, genericDecode)
import Model.ActiveMode (ActiveMode(..))
import Model.SmartHouse.House (JSHouses)
import OBJExporter (MeshFiles, _mtl, _obj)
import Type.Proxy (Proxy(..))
import Web.File (File)


data SavingStep = NotSaving
                | UploadingFiles
                | CreatingHouse
                | WaitingForReady
                | Failed String
                | Finished Int

derive instance genericSavingStep :: Generic SavingStep _
derive instance eqSavingStep :: Eq SavingStep
instance showSavingStep :: Show SavingStep where
    show NotSaving       = "Not saving"
    show UploadingFiles  = "Uploading mesh files for the new house..."
    show CreatingHouse   = "Elli is analyzing the new house data..."
    show WaitingForReady = "Elli is analyzing the new house data..."
    show (Failed msg)    = "Savine house failed: " <> msg
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
uploadMeshFiles :: Int -> MeshFiles -> File -> API (Event (Either MultipleErrors APIResp))
uploadMeshFiles lid m t = formAPI POST ("/v1/lead/" <> show lid <> "/scene/upload") $ mkUploadReq m t

-- API to create manual house/roof data
createManual :: Int -> JSHouses -> API (Event (Either MultipleErrors APIResp))
createManual lid = callAPI POST ("/v1/lead/" <> show lid <> "/create-manual")
--    pure $ pure $ pure $ Left $ singleton $ ForeignError "failed message"


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
instance Default ReadyAPIResp where
    def = ReadyAPIResp {
        success : false,
        houseId : 0
    }

fieldTrans :: String -> String
fieldTrans "success" = "ready"
fieldTrans "houseId" = "house_id"
fieldTrans _         = ""


_success :: forall t a r. Newtype t { success :: a | r } => Lens' t a
_success = _Newtype <<< prop (Proxy :: Proxy "success")


succeeded :: Either MultipleErrors ReadyAPIResp -> Boolean
succeeded (Left _)     = false
succeeded (Right resp) = resp ^. _success

-- API to check if the house is aready or not
checkReady :: Int -> API (Event (Either MultipleErrors ReadyAPIResp))
checkReady lid = callAPI POST ("/v1/lead/" <> show lid <> "/ready") {}

-- check if the 2d house is ready or not repeatedly with a 2 seconds sleep, until it's ready.
repeatCheckUntilReady :: Int -> API (Event (Either MultipleErrors ReadyAPIResp))
repeatCheckUntilReady lid = do
    re <- multicast <$> checkReady lid
    let se = filter succeeded re
        fe = filter (not <<< succeeded) re

    ee <- performAPIEvent $ const (repeatCheckUntilReady lid) <$> delay 2000 fe
    pure $ se <|> keepLatest ee
