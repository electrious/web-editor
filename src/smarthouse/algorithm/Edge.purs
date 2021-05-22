module SmartHouse.Algorithm.Edge where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Lens (Lens', view, (^.))
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy(..))
import Data.UUID (UUID, genUUID)
import Editor.Common.Lenses (_id, _position)
import Effect (Effect)
import Math.LineSeg (LineSeg, mkLineSeg)
import Model.UUID (class HasUUID, idLens)
import SmartHouse.Algorithm.Ray (Ray)
import SmartHouse.Algorithm.VertInfo (VertInfo, _bisector)
import Three.Math.Vector (Vector3)

newtype Edge = Edge {
    id            :: UUID,
    line          :: LineSeg Vector3,
    leftBisector  :: Ray,
    rightBisector :: Ray
    }

derive instance newtypeEdge :: Newtype Edge _
derive instance genericEdge :: Generic Edge _
instance eqEdge :: Eq Edge where
    eq e1 e2 = e1 ^. idLens == e2 ^. idLens
instance ordEdge :: Ord Edge where
    compare = comparing (view idLens)
instance showEdge :: Show Edge where
    show = genericShow
instance hasUUIDEdge :: HasUUID Edge where
    idLens = _id

_line :: forall t a r. Newtype t { line :: a | r } => Lens' t a
_line = _Newtype <<< prop (SProxy :: SProxy "line")

_leftBisector :: forall t a r. Newtype t { leftBisector :: a | r } => Lens' t a
_leftBisector = _Newtype <<< prop (SProxy :: SProxy "leftBisector")

_rightBisector :: forall t a r. Newtype t { rightBisector :: a | r } => Lens' t a
_rightBisector = _Newtype <<< prop (SProxy :: SProxy "rightBisector")

edge :: VertInfo -> VertInfo -> Effect Edge
edge lv rv = do
    i <- genUUID
    pure $ Edge {
        id            : i,
        line          : mkLineSeg (lv ^. _position) (rv ^. _position),
        leftBisector  : lv ^. _bisector,
        rightBisector : rv ^. _bisector
        }
