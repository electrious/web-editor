module Model.Racking.XR10.Splice where

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

newtype Splice = Splice {
    id          :: UUID,
    x           :: Meter,
    y           :: Meter,
    z           :: Meter,
    arrayNumber :: Int,
    railIds     :: Array UUID
}

derive instance newtypeSplice :: Newtype Splice _
derive instance genericSplice :: Generic Splice _
instance showSplice :: Show Splice where
    show = genericShow
instance roofComponentSplice :: RoofComponent Splice where
    compId = view _id
    compX  = view _x
    compY  = view _y
    compZ  = view _z
    size _ = def # _width  .~ inch 7.0
                 # _height .~ inch 1.0
instance arrayComponentSplice :: ArrayComponent Splice where
    arrayNumber = view _arrayNumber
