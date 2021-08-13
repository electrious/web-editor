module Model.Racking.XRFlat.QBaseMount where

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

newtype QBaseMount = QBaseMount {
    id          :: UUID,
    x           :: Meter,
    y           :: Meter,
    z           :: Meter,
    arrayNumber :: Int,
    height      :: Meter
}

derive instance newtypeQBaseMount :: Newtype QBaseMount _
derive instance genericQBaseMount :: Generic QBaseMount _
instance showQBaseMount :: Show QBaseMount where
    show = genericShow
instance roofComponentQBaseMount :: RoofComponent QBaseMount where
    compId = view _id
    compX  = view _x
    compY  = view _y
    compZ  = view _z
    size _ = def # _width  .~ inch 1.0
                 # _height .~ inch 1.0
instance arrayComponentQBaseMount :: ArrayComponent QBaseMount where
    arrayNumber = view _arrayNumber
