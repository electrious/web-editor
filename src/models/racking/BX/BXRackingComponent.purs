module Model.Racking.BX.BXRackingComponent where

import Prelude

import Data.Argonaut.Core (jsonEmptyObject)
import Data.Argonaut.Decode (class DecodeJson, decodeJson, (.:))
import Data.Argonaut.Encode (class EncodeJson, (:=), (~>))
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
import Model.Racking.BX.Block (Block)
import Model.Racking.BX.Chassis (Chassis)

newtype BXRackingComponent = BXRackingComponent {
    arrayNumber :: Int,
    chassis     :: Array Chassis,
    blocks      :: Array Block
}

derive instance Newtype BXRackingComponent _
derive instance Generic BXRackingComponent _
instance Show BXRackingComponent where
    show = genericShow
instance EncodeJson BXRackingComponent where
    encodeJson (BXRackingComponent c) = "an" := c.arrayNumber
                                     ~> "cs" := c.chassis
                                     ~> "bs" := c.blocks
                                     ~> jsonEmptyObject
instance DecodeJson BXRackingComponent where
    decodeJson = decodeJson >=> f
        where f o = mkBXRackingComponent <$> o .: "an"
                                         <*> o .: "cs"
                                         <*> o .: "bs"

mkBXRackingComponent :: Int -> Array Chassis -> Array Block -> BXRackingComponent
mkBXRackingComponent arrayNumber chassis blocks = BXRackingComponent {
    arrayNumber : arrayNumber,
    chassis     : chassis,
    blocks      : blocks
    }


newtype BXRackingNumbers = BXRackingNumbers {
    chassis :: Int,
    blocks  :: Int
}

derive instance Newtype BXRackingNumbers _
derive instance Generic BXRackingNumbers _
instance Show BXRackingNumbers where
    show = genericShow
