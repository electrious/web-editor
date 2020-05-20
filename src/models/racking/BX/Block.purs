module Model.Racking.BX.Block where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Meter (Meter)
import Data.Newtype (class Newtype)
import Data.UUID (UUID)

newtype Block = Block {
    id          :: UUID,
    x           :: Meter,
    y           :: Meter,
    z           :: Meter,
    arrayNumber :: Int
}

derive instance newtypeBlock :: Newtype Block _
derive instance genericBlock :: Generic Block _
instance showBlock :: Show Block where
    show = genericShow
