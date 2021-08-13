module Model.Racking.Flash where

import Prelude

import Data.Default (def)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.Lens (view, (.~))
import Data.Meter (Meter, inch)
import Data.Newtype (class Newtype)
import Data.UUIDWrapper (UUID)
import Editor.Common.Lenses (_arrayNumber, _height, _id, _width, _x, _y, _z)
import Model.ArrayComponent (class ArrayComponent)
import Model.RoofComponent (class RoofComponent)


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
instance roofComponentFlash :: RoofComponent Flash where
    compId = view _id
    compX  = view _x
    compY  = view _y
    compZ  = view _z
    size _ = def # _width  .~ inch 1.0
                 # _height .~ inch 1.0
instance arrayComponentFlash :: ArrayComponent Flash where
    arrayNumber = view _arrayNumber
