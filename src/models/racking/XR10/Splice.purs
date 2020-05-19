module Model.Racking.XR10.Splice where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Meter (Meter)
import Data.Newtype (class Newtype)
import Data.UUID (UUID)

newtype Splice = Splice {
    id          :: UUID,
    x           :: Meter,
    y           :: Meter,
    z           :: Meter,
    arrayNumber :: Int,
    railIds     :: Array UUID
}

derive instance newtypeSplice :: Newtype Splice _
derive instance genericSplice :: Generic Splice _
instance showSplice :: Show Splice where
    show = genericShow
