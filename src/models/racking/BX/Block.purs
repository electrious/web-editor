module Model.Racking.BX.Block where

import Prelude

import Data.Default (def)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.Lens (view, (.~))
import Data.Meter (Meter, meter)
import Data.Newtype (class Newtype)
import Data.UUID (UUID)
import Editor.Common.Lenses (_arrayNumber, _height, _id, _width, _x, _y, _z)
import Model.ArrayComponent (class ArrayComponent)
import Model.RoofComponent (class RoofComponent)

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
instance roofComponentBlock :: RoofComponent Block where
    compId = view _id
    compX  = view _x
    compY  = view _y
    compZ  = view _z
    size _ = def # _width  .~ meter 0.2
                 # _height .~ meter 0.2
instance arrayComponentBlock :: ArrayComponent Block where
    arrayNumber = view _arrayNumber
