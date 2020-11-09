module Model.HouseBuilder.Ridge where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Lens (Lens', (^.))
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy(..))
import Math.Line (Line, mkLine)
import Model.HouseEditor.HousePoint (HousePoint, pointPos)

data RidgeType = TopRidge   -- Ridge connects two RidgePoints
               | SideRidge  -- Ridge connects a RidgePoint and a GutterPoint
               | Gutter     -- Gutter connects two GutterPoints

derive instance genericRidgeType :: Generic RidgeType _
derive instance eqRidgeType :: Eq RidgeType
instance showRidgeType :: Show RidgeType where
    show = genericShow


newtype Ridge = Ridge {
    ridgeType :: RidgeType,
    point1    :: HousePoint,
    point2    :: HousePoint
}

derive instance genericRidge :: Generic Ridge _
derive instance newtypeRidge :: Newtype Ridge _
derive instance eqRidge :: Eq Ridge

_ridgeType :: forall t a r. Newtype t { ridgeType :: a | r } => Lens' t a
_ridgeType = _Newtype <<< prop (SProxy :: SProxy "ridgeType")

_point1 :: forall t a r. Newtype t { point1 :: a | r } => Lens' t a
_point1 = _Newtype <<< prop (SProxy :: SProxy "point1")

_point2 :: forall t a r. Newtype t { point2 :: a | r } => Lens' t a
_point2 = _Newtype <<< prop (SProxy :: SProxy "point2")

mkRidge :: RidgeType -> HousePoint -> HousePoint -> Ridge
mkRidge t p1 p2 = Ridge { ridgeType : t, point1 : p1, point2 : p2 }

ridgeLine :: Ridge -> Line
ridgeLine r = mkLine v1 v2
    where v1 = pointPos $ r ^. _point1
          v2 = pointPos $ r ^. _point2
