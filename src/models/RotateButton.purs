module Model.RotateButton where

import Prelude

import Data.Default (def)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Lens (view, (.~))
import Data.Meter (Meter, meter)
import Data.Newtype (class Newtype)
import Data.UUID (UUID)
import Editor.Common.Lenses (_arrayNumber, _height, _id, _width, _x, _y, _z)
import Math.Angle (Angle)
import Model.ArrayComponent (class ArrayComponent)
import Model.RoofComponent (class RoofComponent)

newtype RotateButton = RotateButton {
    id          :: UUID,
    x           :: Meter,
    y           :: Meter,
    z           :: Meter,
    arrayNumber :: Int,
    rowNumber   :: Int,
    slope       :: Angle
}

derive instance newtypeRotateButton :: Newtype RotateButton _
derive instance genericRotateButton :: Generic RotateButton _
instance showRotateButton :: Show RotateButton where
    show = genericShow
instance roofComponentRotateButton :: RoofComponent RotateButton where
    compId = view _id
    compX  = view _x
    compY  = view _y
    compZ  = view _z
    size _ = def # _width  .~ meter 1.0
                 # _height .~ meter 1.0
instance arrayComponentRotateButton :: ArrayComponent RotateButton where
    arrayNumber = view _arrayNumber