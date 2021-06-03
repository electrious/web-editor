module Model.RotateButton where

import Prelude hiding (degree)

import Data.Default (class Default, def)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Lens (view, (.~), (^.))
import Data.Maybe (Maybe(..))
import Data.Meter (Meter, meter, meterVal)
import Data.Newtype (class Newtype)
import Data.UUID (UUID, emptyUUID)
import Editor.Common.Lenses (_arrayNumber, _height, _id, _width, _x, _y, _z)
import Math.Angle (Angle, degree, radianVal, sin)
import Model.ArrayComponent (class ArrayComponent)
import Model.PlusButton (plusBtnZ)
import Model.Roof.Panel (validatedSlope)
import Model.RoofComponent (class RoofComponent, size)
import Model.UUID (class HasUUID)
import Three.Math.Euler (Euler, mkEuler)
import Three.Math.Vector (Vector3, mkVec3)

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


btnRotation :: RotateButton -> Euler
btnRotation rb = case validatedSlope rb of
    Just slope -> mkEuler (radianVal slope) 0.0 0.0
    Nothing    -> def

btnPosition :: RotateButton -> Vector3
btnPosition rb = let x = meterVal $ rb ^. _x
                     y = meterVal $ rb ^. _y
                     z = meterVal $ rb ^. _z
                 in case validatedSlope rb of
                     Just slope -> let h = meterVal ((size rb) ^. _height) * 0.5 * sin slope
                                   in mkVec3 x y (z + h)
                     Nothing    -> mkVec3 x y z
