module Model.Racking.GAF.GAFRackingComponent where

import Prelude

import Data.Argonaut.Core (jsonEmptyObject)
import Data.Argonaut.Decode (class DecodeJson, decodeJson, (.:))
import Data.Argonaut.Encode (class EncodeJson, (:=), (~>))
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
import Model.Racking.GAF.Hood (Hood)

newtype GAFRackingComponent = GAFRackingComponent {
    arrayNumber :: Int,
    hoods       :: Array Hood
}

derive instance Newtype GAFRackingComponent _
derive instance Generic GAFRackingComponent _
instance Show GAFRackingComponent where
    show = genericShow
instance EncodeJson GAFRackingComponent where
    encodeJson (GAFRackingComponent c) = "an" := c.arrayNumber
                                      ~> "hs" := c.hoods
                                      ~> jsonEmptyObject
instance DecodeJson GAFRackingComponent where
    decodeJson = decodeJson >=> f
        where f o = mkGAFrackingComponent <$> o .: "an"
                                          <*> o .: "hs"

mkGAFrackingComponent :: Int -> Array Hood -> GAFRackingComponent
mkGAFrackingComponent arrayNumber hoods = GAFRackingComponent { arrayNumber : arrayNumber, hoods : hoods }

newtype GAFRackingNumbers = GAFRackingNumbers {
    hoods :: Int
}

derive instance Newtype GAFRackingNumbers _
derive instance Generic GAFRackingNumbers _
instance Show GAFRackingNumbers where
    show = genericShow
