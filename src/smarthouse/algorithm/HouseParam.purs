module SmartHouse.Algorithm.HouseParam where

import Prelude

import Data.Array (zipWith)
import Data.Array as Arr
import Data.Default (class Default, def)
import Data.Lens (view, (.~), (^.))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (class Newtype)
import Data.Triple (Triple(..))
import Editor.Common.Lenses (_edges, _position, _rightEdge, _slope, _vertices)
import Math.LineSeg (mkLineSeg)
import Model.Polygon (Polygon, normalizeContour, polyWindows)
import Model.UUID (idLens)
import SmartHouse.Algorithm.Edge (Edge(..))
import SmartHouse.Algorithm.EdgeInfo (mkEdgeInfo)
import SmartHouse.Algorithm.VertInfo (VertInfo, VertWithSlope, _bisector, vertInfoFrom)
import SmartHouse.Algorithm.VertNode (VertNode(..))
import Three.Math.Vector (mkVec3, normal, vecX, vecY)

newtype HouseParam = HouseParam {
    vertices :: Array VertInfo,
    edges    :: Array Edge
}

derive instance Newtype HouseParam _
instance Default HouseParam where
    def = HouseParam {
        vertices : [],
        edges    : []
    }

mkVi :: Triple VertWithSlope VertWithSlope VertWithSlope -> VertInfo
mkVi (Triple prev p next) = vertInfoFrom (p ^. idLens) pp Nothing 0.0 leftE rightE Nothing Nothing
    where prevP = prev ^. _position
          pp    = p ^. _position
          nextP = next ^. _position

          leftE = mkEdgeInfo (mkLineSeg prevP pp) (prev ^. _slope)
          rightE = mkEdgeInfo (mkLineSeg pp nextP) (p ^. _slope)

vertNodeFromVertInfo :: VertInfo -> VertNode
vertNodeFromVertInfo vi = VertNode {
        id : vi ^. idLens,
        position : vi ^. _position,
        height : 0.0
    }

edge :: VertInfo -> VertInfo -> Edge
edge lv rv =
    let lp = lv ^. _position
        rp = rv ^. _position

        dx = vecX rp - vecX lp
        dy = vecY rp - vecY lp

        n = normal $ mkVec3 dy (-dx) 0.0
    in Edge {
        id            : lv ^. idLens,
        line          : lv ^. _rightEdge,
        leftVertex    : vertNodeFromVertInfo lv,
        rightVertex   : vertNodeFromVertInfo rv,
        leftBisector  : lv ^. _bisector,
        rightBisector : rv ^. _bisector,
        normal        : n
        }

houseParamFrom :: Polygon VertWithSlope -> HouseParam
houseParamFrom poly = def # _vertices .~ vis
                          # _edges    .~ edges
    where vis = mkVi <$> polyWindows (normalizeContour (view _position) poly)
          ns = fromMaybe vis $ Arr.snoc <$> Arr.tail vis <*> Arr.head vis
          edges = zipWith edge vis ns
