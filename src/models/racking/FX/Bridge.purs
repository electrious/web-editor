module Model.Racking.FX.Bridge where

import Prelude

import Data.Default (def)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.Lens (view, (.~))
import Data.Meter (Meter, inch)
import Data.Newtype (class Newtype)
import Data.UUID (UUID)
import Editor.Common.Lenses (_arrayNumber, _height, _id, _width, _x, _y, _z)
import Model.ArrayComponent (class ArrayComponent)
import Model.RoofComponent (class RoofComponent)

bridgeWidth :: Meter
bridgeWidth = inch 8.0

newtype Bridge = Bridge {
    id :: UUID,
    x  :: Meter,
    y  :: Meter,
    z  :: Meter,
    arrayNumber :: Int
}

derive instance newtypeBridge :: Newtype Bridge _
derive instance genericBridge :: Generic Bridge _
instance showBridge :: Show Bridge where
    show = genericShow
instance roofComponentBridge :: RoofComponent Bridge where
    compId = view _id
    compX  = view _x
    compY  = view _y
    compZ  = view _z
    size _ = def # _width  .~ inch 1.0
                 # _height .~ inch 1.0
instance arrayComponentBridge :: ArrayComponent Bridge where
    arrayNumber = view _arrayNumber
