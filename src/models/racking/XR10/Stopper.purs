module Model.Racking.XR10.Stopper where

import Prelude

import Data.Default (def)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.Lens (view, (.~))
import Data.Meter (Meter, meter)
import Data.Newtype (class Newtype)
import Data.UUIDWrapper (UUID)
import Editor.Common.Lenses (_arrayNumber, _height, _id, _width, _x, _y, _z)
import Model.ArrayComponent (class ArrayComponent)
import Model.Racking.Common (RackPos)
import Model.RoofComponent (class RoofComponent)

newtype Stopper = Stopper {
    id          :: UUID,
    x           :: Meter,
    y           :: Meter,
    z           :: Meter,
    arrayNumber :: Int,
    type        :: RackPos
}

derive instance newtypeStopper :: Newtype Stopper _
derive instance genericStopper :: Generic Stopper _
instance showStopper :: Show Stopper where
    show = genericShow
instance roofComponentStopper :: RoofComponent Stopper where
    compId = view _id
    compX  = view _x
    compY  = view _y
    compZ  = view _z
    size _ = def # _width  .~ meter 0.04
                 # _height .~ meter 0.04
instance arrayComponentStopper :: ArrayComponent Stopper where
    arrayNumber = view _arrayNumber
