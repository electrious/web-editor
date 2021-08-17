module Data.UUIDWrapper where

import Prelude

import Data.Argonaut.Decode (class DecodeJson, JsonDecodeError(..), decodeJson)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Either (note)
import Data.Maybe (Maybe)
import Data.UUID as U
import Effect (Effect)

newtype UUID = UUID U.UUID

derive instance Eq UUID
derive instance Ord UUID

instance Show UUID where
    show = toString

instance EncodeJson UUID where
    encodeJson (UUID u) = encodeJson $ U.toString u

instance DecodeJson UUID where
    decodeJson = decodeJson >=> U.parseUUID >>> note (TypeMismatch "not valid UUID string") >>> map UUID

emptyUUID :: UUID
emptyUUID = UUID U.emptyUUID

genUUID :: Effect UUID
genUUID = UUID <$> U.genUUID

parseUUID :: String -> Maybe UUID
parseUUID s = UUID <$> U.parseUUID s

toString :: UUID -> String
toString (UUID u) = U.toString u
