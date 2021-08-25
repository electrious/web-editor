module Model.RotateButton where

import Prelude hiding (degree)

import Data.Default (class Default, def)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.Lens (view, (.~))
import Data.Meter (Meter, meter)
import Data.Newtype (class Newtype)
import Data.UUIDWrapper (UUID, emptyUUID)
import Editor.Common.Lenses (_arrayNumber, _height, _id, _width, _x, _y, _z)
import Math.Angle (Angle, degree)
import Model.ArrayComponent (class ArrayComponent)
import Model.PlusButton (plusBtnZ)
import Model.RoofComponent (class RoofComponent)
import Model.UUID (class HasUUID)

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
instance defaultRotateButton :: Default RotateButton where
    def = RotateButton {
        id          : emptyUUID,
        x           : meter 0.0,
        y           : meter 0.0,
        z           : plusBtnZ,
        arrayNumber : 0,
        rowNumber   : 0,
        slope       : degree 0.0
    }
instance hasUUIDRotateButton :: HasUUID RotateButton where
    idLens = _id
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