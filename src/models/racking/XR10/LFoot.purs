module Model.Racking.XR10.LFoot where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Meter (Meter)
import Data.Newtype (class Newtype)
import Data.UUID (UUID)

newtype LFoot = LFoot {
    id          :: UUID,
    x           :: Meter,
    y           :: Meter,
    z           :: Meter,
    arrayNumber :: Int,
    flashId     :: UUID
}

derive instance newtypeLFoot :: Newtype LFoot _
derive instance genericLFoot :: Generic LFoot _
instance showLFoot :: Show LFoot where
    show = genericShow
