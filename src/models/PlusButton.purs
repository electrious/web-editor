module Model.PlusButton where

import Prelude hiding (degree)

import Data.Default (class Default, def)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Lens (view, (^.), (.~))
import Data.List (List(..), (:))
import Data.Meter (Meter, meter, meterVal)
import Data.Newtype (class Newtype)
import Data.UUID (UUID, emptyUUID)
import Editor.Common.Lenses (_arrayNumber, _height, _id, _orientation, _width, _x, _y, _z)
import Math.Angle (Angle, degree)
import Model.ArrayComponent (class ArrayComponent)
import Model.Roof.Panel (Orientation(..), panelLong, panelShort)
import Model.RoofComponent (class RoofComponent, compX, compY, size)
import Model.UUID (class HasUUID)
import Three.Math.Vector (Vector3, mkVec3)

plusBtnZ :: Meter
plusBtnZ = meter 0.2

newtype PlusButton = PlusButton {
    id          :: UUID,
    x           :: Meter,
    y           :: Meter,
    z           :: Meter,
    orientation :: Orientation,
    slope       :: Angle,
    arrayNumber :: Int
}

derive instance newtypePlusButton :: Newtype PlusButton _
derive instance genericPlusButton :: Generic PlusButton _
instance defaultPlusButton :: Default PlusButton where
    def = PlusButton {
        id          : emptyUUID,
        x           : meter 0.0,
        y           : meter 0.0,
        z           : plusBtnZ,
        orientation : Landscape,
        slope       : degree 0.0,
        arrayNumber : 0
    }
instance hasUUIDPlusButton :: HasUUID PlusButton where
    idLens = _id
instance showPlusButton :: Show PlusButton where
    show = genericShow
instance roofComponentPlusButton :: RoofComponent PlusButton where
    compId = view _id
    compX  = view _x
    compY  = view _y
    compZ  = view _z
    size p = case p ^. _orientation of
        Landscape -> def # _width  .~ panelLong
                         # _height .~ panelShort
        Portrait  -> def # _width  .~ panelShort
                         # _height .~ panelLong
instance arrayComponentPlusButton :: ArrayComponent PlusButton where
    arrayNumber = view _arrayNumber


btnVertices :: PlusButton -> List Vector3
btnVertices pb = v1 : v2 : v3 : v4 : Nil
    where s  = size pb
          x  = meterVal $ compX pb
          y  = meterVal $ compY pb
          w2 = meterVal (s ^. _width) / 2.0
          h2 = meterVal (s ^. _height) / 2.0
          v1 = mkVec3 (x - w2) (y - h2) 0.0
          v2 = mkVec3 (x - w2) (y + h2) 0.0
          v3 = mkVec3 (x + w2) (y + h2) 0.0
          v4 = mkVec3 (x + w2) (y - h2) 0.0
