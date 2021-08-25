module Model.Racking.XR10.XRRackingComponent where

import Prelude

import Data.Argonaut.Core (jsonEmptyObject)
import Data.Argonaut.Decode (class DecodeJson, decodeJson, (.:))
import Data.Argonaut.Encode (class EncodeJson, (:=), (~>))
import Data.Default (class Default)
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
import Model.Racking.Flash (Flash)
import Model.Racking.XR10.Clamp (Clamp)
import Model.Racking.XR10.LFoot (LFoot)
import Model.Racking.XR10.Rail (Rail)
import Model.Racking.XR10.Splice (Splice)
import Model.Racking.XR10.Stopper (Stopper)

newtype XRRackingComponent = XRRackingComponent {
    arrayNumber :: Int,
    flashes     :: Array Flash,
    rails       :: Array Rail,
    railsNum    :: Int,
    splices     :: Array Splice,
    lfeet       :: Array LFoot,
    clamps      :: Array Clamp,
    stoppers    :: Array Stopper
}

derive instance Newtype XRRackingComponent _
derive instance Generic XRRackingComponent _
instance Show XRRackingComponent where
    show = genericShow
instance Default XRRackingComponent where
    def = mkXRRackingComponent 0 [] [] 0 [] [] [] []
instance EncodeJson XRRackingComponent where
    encodeJson (XRRackingComponent c) = "an"  := c.arrayNumber
                                     ~> "fs"  := c.flashes
                                     ~> "rs"  := c.rails
                                     ~> "rn"  := c.railsNum
                                     ~> "ss"  := c.splices
                                     ~> "lf"  := c.lfeet
                                     ~> "cs"  := c.clamps
                                     ~> "sts" := c.stoppers
                                     ~> jsonEmptyObject
instance DecodeJson XRRackingComponent where
    decodeJson = decodeJson >=> f
        where f o = mkXRRackingComponent <$> o .: "an"
                                         <*> o .: "fs"
                                         <*> o .: "rs"
                                         <*> o .: "rn"
                                         <*> o .: "ss"
                                         <*> o .: "lf"
                                         <*> o .: "cs"
                                         <*> o .: "sts"

mkXRRackingComponent :: Int -> Array Flash -> Array Rail -> Int -> Array Splice -> Array LFoot -> Array Clamp -> Array Stopper -> XRRackingComponent
mkXRRackingComponent arrayNumber flashes rails rn splices lfeet clamps stoppers = XRRackingComponent { arrayNumber: arrayNumber, flashes: flashes, rails: rails, railsNum: rn, splices: splices, lfeet: lfeet, clamps: clamps, stoppers: stoppers }

newtype XRRackingNumbers = XRRackingNumbers {
    flashes  :: Int,
    rails    :: Int,
    splices  :: Int,
    lfeet    :: Int,
    clamps   :: Int,
    stoppers :: Int
}

derive instance Newtype XRRackingNumbers _
derive instance Generic XRRackingNumbers _
instance Show XRRackingNumbers where
    show = genericShow
