module Algorithm.TempPanels where

import Prelude

import Data.Array (null)
import Data.Filterable (filter)
import Data.Int (floor, toNumber)
import Data.List (List(..), concatMap, (..))
import Data.Maybe (Maybe(..))
import Data.Meter (meter, meterVal)
import Data.Ord (abs)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Model.Roof.ArrayConfig (ArrayConfig, colDistance, rowDistance)
import Model.Roof.Panel (Panel, clonePanelTo)
import Model.RoofComponent (compBBox)
import RBush.RBush (RBush, search)
import Three.Math.Vector (Vector3, vecX, vecY, (<->))

tempPanels :: ArrayConfig -> RBush Panel -> Panel -> Vector3 -> Vector3 -> Effect (List Panel)
tempPanels cfg tree p start end = filter validP <$> traverse cloneP (concatMap (\x -> Tuple x <$> ys) xs)
    where sx = vecX start
          sy = vecY start

          delta = end <-> start
          deltaX = vecX delta
          deltaY = vecY delta

          pW = meterVal $ colDistance cfg p
          pH = meterVal $ rowDistance cfg p

          numX = floor $ abs deltaX / pW
          numY = floor $ abs deltaY / pH

          xDir = if deltaX > 0.0 then 1.0 else (-1.0)
          yDir = if deltaY > 0.0 then 1.0 else (-1.0)

          cloneP (Tuple x y) = clonePanelTo p x y

          is = if numX > 0 then 0..(numX - 1) else Nil
          js = if numY > 0 then 0..(numY - 1) else Nil
          xs = (\i -> meter $ sx + pW * toNumber i * xDir) <$> is
          ys = (\j -> meter $ sy + pH * toNumber j * yDir) <$> js

          validP panel = null $ search (compBBox panel Nothing) tree