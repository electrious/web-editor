module Model.Racking.Rafter where

import Prelude

import Data.Default (def)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.Lens (view, (.~), (^.))
import Data.Meter (Meter, inch)
import Data.Newtype (class Newtype)
import Data.UUID (UUID)
import Editor.Common.Lenses (_height, _id, _length, _width, _x, _y, _z)
import Model.RoofComponent (class RoofComponent)


newtype Rafter = Rafter {
    id     :: UUID,
    x      :: Meter,
    y      :: Meter,
    z      :: Meter,
    length :: Meter
}

derive instance newtypeRafter :: Newtype Rafter _
derive instance genericRafter :: Generic Rafter _
instance showRafter :: Show Rafter where
    show = genericShow
instance roofComponentRafter :: RoofComponent Rafter where
    compId = view _id
    compX  = view _x
    compY  = view _y
    compZ  = view _z
    size r = def # _width  .~ inch 1.0
                 # _height .~ r ^. _length
