module Model.Racking.OldRackingSystem where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.Int (fromString)
import Data.Lens ((^.))
import Data.List (List)
import Data.Map (Map, fromFoldable, toUnfoldable)
import Data.Maybe (Maybe, fromMaybe, isJust)
import Data.Newtype (class Newtype)
import Data.Tuple (Tuple(..))
import Editor.Common.Lenses (_mountSpacing, _rackingType)
import Foreign.Generic (class Decode, class Encode, decode, defaultOptions, encode, genericDecode, genericEncode)
import Foreign.Object (Object)
import Foreign.Object as Object
import Model.Racking.MountSpacing (MountSpacing)
import Model.Racking.RackingType (RackingType(..))
import Model.Racking.RafterSpacing (RafterSpacing)
import Model.Racking.RoofParameter (RoofParameter)

-- | data type for racking data on specific roofplate in the old racking API
newtype OldRoofRackingData = OldRoofRackingData {
    -- old fields, only read, don't write anymore
    mountSpacing  :: Maybe MountSpacing,
    rafterSpacing :: Maybe RafterSpacing,

    -- new fields to write to server
    rackingType   :: Maybe RackingType,
    parameters    :: Maybe RoofParameter
}

derive instance newtypeOldRoofRackingData :: Newtype OldRoofRackingData _
derive instance genericOldRoofRackingData :: Generic OldRoofRackingData _
instance showOldRoofRackingData :: Show OldRoofRackingData where
    show = genericShow
instance encodeOldRoofRackingData :: Encode OldRoofRackingData where
    encode = genericEncode (defaultOptions { unwrapSingleConstructors = true,
                                             fieldTransform = rackDataFieldTrans })
instance decodeOldRoofRackingData :: Decode OldRoofRackingData where
    decode = genericDecode (defaultOptions { unwrapSingleConstructors = true,
                                             fieldTransform = rackDataFieldTrans })

rackDataFieldTrans :: String -> String
rackDataFieldTrans "mountSpacing"  = "mount_space"
rackDataFieldTrans "rafterSpacing" = "rafter_space"
rackDataFieldTrans "rackingType"   = "rack_type"
rackDataFieldTrans "parameters"    = "param"
rackDataFieldTrans _               = "mount_space"


guessRackingType :: OldRoofRackingData -> RackingType
guessRackingType d = if isJust (d ^. _mountSpacing)
                     then XR10
                     else fromMaybe XR10 (d ^. _rackingType)

newtype OldRackingSystem = OldRackingSystem {
    rackingType  :: RackingType,
    roofRackings :: Map Int OldRoofRackingData
}

derive instance newtypeOldRackingSystem :: Newtype OldRackingSystem _
derive instance genericOldRackingSystem :: Generic OldRackingSystem _
instance showOldRackingSystem :: Show OldRackingSystem where
    show = genericShow
instance encodeOldRackingSystem :: Encode OldRackingSystem where
    encode = toCodable >>> encode
instance decodeOldRackingSystem :: Decode OldRackingSystem where
    decode = decode >>> map fromCodable

newtype OldRackSystemCodable = OldRackSystemCodable {
    rackingType :: RackingType,
    roofRackings :: Object OldRoofRackingData
}
derive instance genericOldRackSystemCodable :: Generic OldRackSystemCodable _
instance encodeOldRackSystemCodable :: Encode OldRackSystemCodable where
    encode = genericEncode (defaultOptions { unwrapSingleConstructors = true,
                                             fieldTransform = fieldTrans
                                           })
instance decodeOldRackSystemCodable :: Decode OldRackSystemCodable where
    decode = genericDecode (defaultOptions { unwrapSingleConstructors = true,
                                             fieldTransform = fieldTrans
                                            })

toCodable :: OldRackingSystem -> OldRackSystemCodable
toCodable (OldRackingSystem { rackingType, roofRackings }) = OldRackSystemCodable {
    rackingType  : rackingType,
    roofRackings : toObject roofRackings
}

fromCodable :: OldRackSystemCodable -> OldRackingSystem
fromCodable (OldRackSystemCodable { rackingType, roofRackings }) = OldRackingSystem {
    rackingType  : rackingType,
    roofRackings : fromObject roofRackings
}

toObject :: Map Int OldRoofRackingData -> Object OldRoofRackingData
toObject = Object.fromFoldable <<< map f <<< (toUnfoldable :: Map Int OldRoofRackingData -> List (Tuple Int OldRoofRackingData))
    where f (Tuple k v) = Tuple (show k) v

fromObject :: Object OldRoofRackingData -> Map Int OldRoofRackingData
fromObject = fromFoldable <<< map f <<< (Object.toUnfoldable :: Object OldRoofRackingData -> List (Tuple String OldRoofRackingData))
    where f (Tuple k v) = Tuple (fromMaybe 0 $ fromString k) v

fieldTrans :: String -> String
fieldTrans "rackingType"  = "type"
fieldTrans "roofRackings" = "components"
fieldTrans _              = ""