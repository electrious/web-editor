module Model.Racking.Flash where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Meter (Meter)
import Data.Newtype (class Newtype)
import Data.UUID (UUID)

newtype Flash = Flash {
    id          :: UUID,
    rafterId    :: UUID,
    x           :: Meter,
    y           :: Meter,
    z           :: Meter,
    arrayNumber :: Int
}

derive instance newtypeFlash :: Newtype Flash _
derive instance genericFlash :: Generic Flash _
instance showFlash :: Show Flash where
    show = genericShow
