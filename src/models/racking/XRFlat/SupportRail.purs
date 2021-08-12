module Model.Racking.XRFlat.SupportRail where

import Prelude

import Data.Default (def)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.Lens (view, (^.), (.~))
import Data.Meter (Meter, inch)
import Data.Newtype (class Newtype)
import Data.UUID (UUID)
import Editor.Common.Lenses (_arrayNumber, _height, _id, _length, _width, _x, _y, _z)
import Model.ArrayComponent (class ArrayComponent)
import Model.RoofComponent (class RoofComponent)

newtype SupportRail = SupportRail {
    id          :: UUID,
    x           :: Meter,
    y           :: Meter,
    z           :: Meter,
    arrayNumber :: Int,
    length      :: Meter
}

derive instance newtypeSupportRail :: Newtype SupportRail _
derive instance genericSupportRail :: Generic SupportRail _
instance showSupportRail :: Show SupportRail where
    show = genericShow
instance roofComponentSupportRail :: RoofComponent SupportRail where
    compId = view _id
    compX  = view _x
    compY  = view _y
    compZ  = view _z
    size s = def # _width  .~ inch 1.0
                 # _height .~ s ^. _length
instance arrayComponentSupportRail :: ArrayComponent SupportRail where
    arrayNumber = view _arrayNumber
