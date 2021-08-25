module Model.Racking.XRFlat.XRFlatRackingComponent where

import Prelude

import Data.Argonaut.Core (jsonEmptyObject)
import Data.Argonaut.Decode (class DecodeJson, decodeJson, (.:))
import Data.Argonaut.Encode (class EncodeJson, (:=), (~>))
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
import Model.Racking.XR10.Clamp (Clamp)
import Model.Racking.XR10.Rail (Rail)
import Model.Racking.XR10.Splice (Splice)
import Model.Racking.XR10.Stopper (Stopper)
import Model.Racking.XRFlat.QBaseMount (QBaseMount)
import Model.Racking.XRFlat.SupportRail (SupportRail)
import Model.Racking.XRFlat.TiltLeg (TiltLeg)

newtype XRFlatRackingComponent = XRFlatRackingComponent {
    arrayNumber  :: Int,
    rails        :: Array Rail,
    railsNum     :: Int,
    splices      :: Array Splice,
    clamps       :: Array Clamp,
    stoppers     :: Array Stopper,
    supportRails :: Array SupportRail,
    baseMounts   :: Array QBaseMount,
    tiltLegs     :: Array TiltLeg
}

derive instance Newtype XRFlatRackingComponent _
derive instance Generic XRFlatRackingComponent _
instance Show XRFlatRackingComponent where
    show = genericShow
instance EncodeJson XRFlatRackingComponent where
    encodeJson (XRFlatRackingComponent c) = "an"  := c.arrayNumber
                                         ~> "rs"  := c.rails
                                         ~> "rn"  := c.railsNum
                                         ~> "ss"  := c.splices
                                         ~> "cs"  := c.clamps
                                         ~> "sts" := c.stoppers
                                         ~> "srs" := c.supportRails
                                         ~> "ms"  := c.baseMounts
                                         ~> "ts"  := c.tiltLegs
                                         ~> jsonEmptyObject
instance DecodeJson XRFlatRackingComponent where
    decodeJson = decodeJson >=> f
        where f o = mkXRFlatRackingComponent <$> o .: "an"
                                             <*> o .: "rs"
                                             <*> o .: "rn"
                                             <*> o .: "ss"
                                             <*> o .: "cs"
                                             <*> o .: "sts"
                                             <*> o .: "srs"
                                             <*> o .: "ms"
                                             <*> o .: "ts"

mkXRFlatRackingComponent :: Int -> Array Rail -> Int -> Array Splice -> Array Clamp -> Array Stopper -> Array SupportRail -> Array QBaseMount -> Array TiltLeg -> XRFlatRackingComponent
mkXRFlatRackingComponent an rs rn ss cs sts srs ms ts = XRFlatRackingComponent {arrayNumber: an, rails: rs, railsNum: rn, splices: ss, clamps: cs, stoppers: sts, supportRails: srs, baseMounts: ms, tiltLegs: ts}


newtype XRFlatRackingNumbers = XRFlatRackingNumbers {
    rails        :: Int,
    splices      :: Int,
    clamps       :: Int,
    stoppers     :: Int,
    supportRails :: Int,
    baseMounts   :: Int,
    tiltLegs     :: Int
}

derive instance Newtype XRFlatRackingNumbers _
derive instance Generic XRFlatRackingNumbers _
instance Show XRFlatRackingNumbers where
    show = genericShow
