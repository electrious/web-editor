module Model.HouseBuilder.Ridge where


import Data.Lens ((^.))
import Math.Line (Line, mkLine)
import Model.HouseEditor.HousePoint (GutterPoint, RidgePoint, _gutterPointPos, _ridgePointPos)


data Ridge = TopRidge  RidgePoint RidgePoint
           | SideRidge RidgePoint GutterPoint


topRidge :: RidgePoint -> RidgePoint -> Ridge
topRidge p1 p2 = TopRidge p1 p2

sideRidge :: RidgePoint -> GutterPoint -> Ridge
sideRidge p1 p2 = SideRidge p1 p2

ridgeLine :: Ridge -> Line
ridgeLine (TopRidge p1 p2)  = mkLine (p1 ^. _ridgePointPos) (p2 ^. _ridgePointPos)
ridgeLine (SideRidge p1 p2) = mkLine (p1 ^. _ridgePointPos) (p2 ^. _gutterPointPos)
