module Algorithm.TempPanels where

import Prelude

import Data.Int (floor, toNumber)
import Data.List (List, concatMap, (..))
import Data.Meter (meter, meterVal)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Math (abs)
import Model.Roof.ArrayConfig (ArrayConfig, colDistance, rowDistance)
import Model.Roof.Panel (Panel, clonePanelTo)
import Three.Math.Vector (Vector3, vecX, vecY, (<->))

tempPanels :: ArrayConfig -> Panel -> Vector3 -> Vector3 -> Effect (List Panel)
tempPanels cfg p start end = traverse cloneP $ concatMap (\x -> Tuple x <$> ys) xs
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

          xs = (\i -> meter $ sx + pW * toNumber i * xDir) <$> 0..(numX - 1)
          ys = (\j -> meter $ sy + pH * toNumber j * yDir) <$> 0..(numY - 1)
