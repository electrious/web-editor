module API.SmartHouse where

import Prelude

import API (API, APIError, callAPI, formAPI, performAPIEvent)
import Control.Alt ((<|>))
import Data.Argonaut.Decode (class DecodeJson, decodeJson, (.:))
import Data.Default (class Default)
import Data.Either (Either(..))
import Data.Filterable (filter)
import Data.Generic.Rep (class Generic)
import Data.HTTP.Method (Method(..))
import Data.Lens (Lens', (^.))
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Class (liftEffect)
import FRP.Event (Event, keepLatest)
import FRP.Event.Extra (delay, multicast)
import Model.ActiveMode (ActiveMode(..))
import Model.SmartHouse.House (JSHouses)
import OBJExporter (MeshFiles, _mtl, _obj)
import Type.Proxy (Proxy(..))
import Web.File.File (File, toBlob)
import Web.XHR.FormData (EntryName(..), FormData, appendBlob, new)


data SavingStep = NotSaving
                | UploadingFiles
                | CreatingHouse
                | WaitingForReady
                | Failed String
                | Finished Int Int

derive instance Generic SavingStep _
derive instance Eq SavingStep
instance Show SavingStep where
    show NotSaving       = "Not saving"
    show UploadingFiles  = "Uploading mesh files for the new house..."
    show CreatingHouse   = "Elli is analyzing the new house data..."
    show WaitingForReady = "Elli is analyzing the new house data..."
    show (Failed msg)    = "Savine house failed: " <> msg
    show (Finished _ _)  = "Finished creating the new house"

stepMode :: SavingStep -> ActiveMode
stepMode NotSaving      = Inactive
stepMode (Finished _ _) = Inactive
stepMode _              = Active

isFinished :: SavingStep -> Boolean
isFinished (Finished _ _) = true
isFinished _              = false

savedHouseId :: SavingStep -> Maybe (Tuple Int Int)
savedHouseId (Finished h c) = Just $ Tuple h c
savedHouseId _              = Nothing


newtype UploadReq = UploadReq {
    obj     :: File,
    mtl     :: File,
    texture :: File
    }

derive instance newtypeUploadReq :: Newtype UploadReq _

mkUploadReq :: MeshFiles -> File -> UploadReq
mkUploadReq m t = UploadReq {
    obj     : m ^. _obj,
    mtl     : m ^. _mtl,
    texture : t
    }

toFormData :: UploadReq -> Effect FormData
toFormData (UploadReq r) = do
    fd <- new
    appendBlob (EntryName "obj") (toBlob r.obj) Nothing fd
    appendBlob (EntryName "mtl") (toBlob r.mtl) Nothing fd
    appendBlob (EntryName "texture") (toBlob r.texture) Nothing fd
    pure fd

newtype APIResp = APIResp {
    success :: Boolean
    }

derive instance Newtype APIResp _
derive instance Generic APIResp _
instance DecodeJson APIResp where
    decodeJson = decodeJson >=> f
        where f o = mkAPIResp <$> o .: "success"

mkAPIResp :: Boolean -> APIResp
mkAPIResp s = APIResp { success : s }

-- API to upload obj/mtl/texture files
uploadMeshFiles :: Int -> MeshFiles -> File -> API (Event (Either APIError APIResp))
uploadMeshFiles lid m t = liftEffect (toFormData (mkUploadReq m t)) >>= formAPI POST ("/v1/lead/" <> show lid <> "/scene/upload")

-- API to create manual house/roof data
createManual :: Int -> JSHouses -> API (Event (Either APIError APIResp))
createManual lid = callAPI POST ("/v1/lead/" <> show lid <> "/create-manual")
--    pure $ pure $ pure $ Left $ singleton $ ForeignError "failed message"


newtype ReadyAPIResp = ReadyAPIResp {
    success   :: Boolean,
    houseId   :: Int,
    companyId :: Int
    }

derive instance Newtype ReadyAPIResp _
derive instance Generic ReadyAPIResp _
instance DecodeJson ReadyAPIResp where
    decodeJson = decodeJson >=> f
        where f o = mkReadyAPIResp <$> o .: "ready"
                                   <*> o .: "house_id"
                                   <*> o .: "company_id"

mkReadyAPIResp :: Boolean -> Int -> Int -> ReadyAPIResp
mkReadyAPIResp success houseId companyId = ReadyAPIResp { success, houseId, companyId }

instance Default ReadyAPIResp where
    def = ReadyAPIResp {
        success   : false,
        houseId   : 0,
        companyId : 1
    }


_success :: forall t a r. Newtype t { success :: a | r } => Lens' t a
_success = _Newtype <<< prop (Proxy :: Proxy "success")

_companyId :: forall t a r. Newtype t { companyId :: a | r } => Lens' t a
_companyId = _Newtype <<< prop (Proxy :: Proxy "companyId")

succeeded :: Either APIError ReadyAPIResp -> Boolean
succeeded (Left _)     = false
succeeded (Right resp) = resp ^. _success

-- API to check if the house is aready or not
checkReady :: Int -> API (Event (Either APIError ReadyAPIResp))
checkReady lid = callAPI POST ("/v1/lead/" <> show lid <> "/ready") {}

-- check if the 2d house is ready or not repeatedly with a 2 seconds sleep, until it's ready.
repeatCheckUntilReady :: Int -> API (Event (Either APIError ReadyAPIResp))
repeatCheckUntilReady lid = do
    re <- multicast <$> checkReady lid
    let se = filter succeeded re
        fe = filter (not <<< succeeded) re

    ee <- performAPIEvent $ const (repeatCheckUntilReady lid) <$> delay 2000 fe
    pure $ se <|> keepLatest ee
