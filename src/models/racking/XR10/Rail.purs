module Model.Racking.XR10.Rail where

import Prelude

import Data.Default (def)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.Lens (view, (^.), (.~))
import Data.Meter (Meter, feetInch, inch)
import Data.Newtype (class Newtype)
import Data.UUID (UUID)
import Editor.Common.Lenses (_arrayNumber, _height, _id, _length, _width, _x, _y, _z)
import Model.ArrayComponent (class ArrayComponent)
import Model.RoofComponent (class RoofComponent)


railLength :: Meter
railLength = feetInch 14.0 0.0

newtype Rail = Rail {
    id          :: UUID,
    x           :: Meter,
    y           :: Meter,
    z           :: Meter,
    arrayNumber :: Int,
    length      :: Meter,
    panelIds    :: Array UUID
}

derive instance newtypeRail :: Newtype Rail _
derive instance genericRail :: Generic Rail _
instance showRail :: Show Rail where
    show = genericShow
instance roofComponentRail :: RoofComponent Rail where
    compId = view _id
    compX  = view _x
    compY  = view _y
    compZ  = view _z
    size r = def # _width  .~ r ^. _length
                 # _height .~ inch 1.0
instance arrayComponentRail :: ArrayComponent Rail where
    arrayNumber = view _arrayNumber
