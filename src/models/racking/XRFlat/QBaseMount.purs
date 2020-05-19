module Model.Racking.XRFlat.QBaseMount where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Meter (Meter)
import Data.Newtype (class Newtype)
import Data.UUID (UUID)

newtype QBaseMount = QBaseMount {
    id          :: UUID,
    x           :: Meter,
    y           :: Meter,
    z           :: Meter,
    arrayNumber :: Int,
    height      :: Meter
}

derive instance newtypeQBaseMount :: Newtype QBaseMount _
derive instance genericQBaseMount :: Generic QBaseMount _
instance showQBaseMount :: Show QBaseMount where
    show = genericShow
