module Model.RoofComponent where

import Prelude hiding (degree)

import Data.Default (class Default, def)
import Data.Hardware.Size (Size)
import Data.Lens ((^.), (.~))
import Data.Maybe (Maybe, fromMaybe)
import Data.Meter (Meter, meterVal)
import Data.UUID (UUID)
import Editor.Common.Lenses (_height, _item, _maxX, _maxY, _minX, _minY, _width)
import Math.Angle (Angle, cos, degree)
import RBush.RBush (BBox)

class RoofComponent a where
    compId :: a -> UUID
    compX  :: a -> Meter
    compY  :: a -> Meter
    compZ  :: a -> Meter
    size   :: a -> Size

-- | calculate Bounding Box of a component
compBBox :: forall a. RoofComponent a => Default a => a -> Maybe Angle -> BBox a
compBBox c slope = def # _minX .~ x - w2
                       # _minY .~ y - h2
                       # _maxX .~ x + w2
                       # _maxY .~ y + h2
                       # _item .~ c
    where s = size c
          x = meterVal $ compX c
          y = meterVal $ compY c
          w2 = meterVal (s ^. _width) / 2.0
          h2 = (meterVal $ s ^. _height) * cos (fromMaybe (degree 0.0) slope) / 2.0

compBBoxWithOffset :: forall a. RoofComponent a => Default a => a -> Number -> Number -> Maybe Angle -> BBox a
compBBoxWithOffset c xOffset yOffset slope = def # _minX .~ x - w2
                                                 # _minY .~ y - h2
                                                 # _maxX .~ x + w2
                                                 # _maxY .~ y + h2
                                                 # _item .~ c
    where s = size c
          x = meterVal $ compX c
          y = meterVal $ compY c
          w2 = meterVal (s ^. _width) / 2.0 + xOffset
          h2 = meterVal (s ^. _height) * cos (fromMaybe (degree 0.0) slope) / 2.0 + yOffset
