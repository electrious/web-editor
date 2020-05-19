module Model.Racking.FX.EndCap where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Meter (Meter)
import Data.Newtype (class Newtype)
import Data.UUID (UUID)
import Model.Racking.Common (RackPos)

newtype EndCap = EndCap {
    id          :: UUID,
    x           :: Meter,
    y           :: Meter,
    z           :: Meter,
    arrayNumber :: Int,
    skirtId     :: UUID,
    position    ::  RackPos
}

derive instance newtypeEndCap :: Newtype EndCap _
derive instance genericEndCap :: Generic EndCap _
instance showEndCap :: Show EndCap where
    show = genericShow
