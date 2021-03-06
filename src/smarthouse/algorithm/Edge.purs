module SmartHouse.Algorithm.Edge where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.Lens (Lens', view, (^.))
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Newtype (class Newtype)
import Type.Proxy (Proxy(..))
import Data.UUID (UUID, genUUID)
import Editor.Common.Lenses (_id, _position)
import Effect (Effect)
import Math.LineSeg (LineSeg, mkLineSeg)
import Model.UUID (class HasUUID, idLens)
import SmartHouse.Algorithm.Ray (Ray)
import SmartHouse.Algorithm.VertInfo (VertInfo, _bisector)
import Three.Math.Vector (Vector3, mkVec3, normal, vecX, vecY)

newtype Edge = Edge {
    id            :: UUID,
    line          :: LineSeg Vector3,
    leftVertex    :: Vector3,
    rightVertex   :: Vector3,
    leftBisector  :: Ray,
    rightBisector :: Ray,

    normal        :: Vector3  -- outward normal vector for an edge
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
_line = _Newtype <<< prop (Proxy :: Proxy "line")

_leftVertex :: forall t a r. Newtype t { leftVertex :: a | r } => Lens' t a
_leftVertex = _Newtype <<< prop (Proxy :: Proxy "leftVertex")

_rightVertex :: forall t a r. Newtype t { rightVertex :: a | r } => Lens' t a
_rightVertex = _Newtype <<< prop (Proxy :: Proxy "rightVertex")

_leftBisector :: forall t a r. Newtype t { leftBisector :: a | r } => Lens' t a
_leftBisector = _Newtype <<< prop (Proxy :: Proxy "leftBisector")

_rightBisector :: forall t a r. Newtype t { rightBisector :: a | r } => Lens' t a
_rightBisector = _Newtype <<< prop (Proxy :: Proxy "rightBisector")

edge :: VertInfo -> VertInfo -> Effect Edge
edge lv rv = do
    i <- genUUID
    let lp = lv ^. _position
        rp = rv ^. _position

        dx = vecX rp - vecX lp
        dy = vecY rp - vecY lp

        n = normal $ mkVec3 dy (-dx) 0.0
    pure $ Edge {
        id            : i,
        line          : mkLineSeg lp rp,
        leftVertex    : lp,
        rightVertex   : rp,
        leftBisector  : lv ^. _bisector,
        rightBisector : rv ^. _bisector,
        normal        : n
        }
