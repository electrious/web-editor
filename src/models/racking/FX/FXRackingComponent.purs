module Model.Racking.FX.FXRackingComponent where

import Prelude

import Data.Argonaut.Core (jsonEmptyObject)
import Data.Argonaut.Decode (class DecodeJson, decodeJson, (.:))
import Data.Argonaut.Encode (class EncodeJson, (:=), (~>))
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
import Model.Racking.FX.Bridge (Bridge)
import Model.Racking.FX.EndCap (EndCap)
import Model.Racking.FX.Mount (Mount)
import Model.Racking.FX.Skirt (Skirt)
import Model.Racking.Flash (Flash)


newtype FXRackingComponent = FXRackingComponent {
    arrayNumber  :: Int,
    flashes      :: Array Flash,
    mounts       :: Array Mount,
    bridges      :: Array Bridge,
    skirts       :: Array Skirt,
    leftEndCaps  :: Array EndCap,
    rightEndCaps :: Array EndCap
}

derive instance Newtype FXRackingComponent _
derive instance Generic FXRackingComponent _
instance Show FXRackingComponent where
    show = genericShow
instance EncodeJson FXRackingComponent where
    encodeJson (FXRackingComponent c) = "an" := c.arrayNumber
                                     ~> "fs" := c.flashes
                                     ~> "ms" := c.mounts
                                     ~> "bs" := c.bridges
                                     ~> "ss" := c.skirts
                                     ~> "lcs" := c.leftEndCaps
                                     ~> "rcs" := c.rightEndCaps
                                     ~> jsonEmptyObject
instance DecodeJson FXRackingComponent where
    decodeJson = decodeJson >=> f
        where f o = mkFXRackkingComponent <$> o .: "an"
                                          <*> o .: "fs"
                                          <*> o .: "ms"
                                          <*> o .: "bs"
                                          <*> o .: "ss"
                                          <*> o .: "lcs"
                                          <*> o .: "rcs"

mkFXRackkingComponent :: Int -> Array Flash -> Array Mount -> Array Bridge -> Array Skirt -> Array EndCap -> Array EndCap -> FXRackingComponent
mkFXRackkingComponent arrayNumber flashes mounts bridges skirts leftEndCaps rightEndCaps = FXRackingComponent {
    arrayNumber  : arrayNumber,
    flashes      : flashes,
    mounts       : mounts,
    bridges      : bridges,
    skirts       : skirts,
    leftEndCaps  : leftEndCaps,
    rightEndCaps : rightEndCaps
}

newtype FXRackingNumbers = FXRackingNumbers {
    flashes      :: Int,
    mounts       :: Int,
    bridges      :: Int,
    skirts       :: Int,
    leftEndCaps  :: Int,
    rightEndCaps :: Int
}

derive instance Newtype FXRackingNumbers _
derive instance Generic FXRackingNumbers _
instance Show FXRackingNumbers where
    show = genericShow
