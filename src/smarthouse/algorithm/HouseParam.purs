module SmartHouse.Algorithm.HouseParam where

import Prelude

import Data.Array (zipWith)
import Data.Array as Arr
import Data.Default (class Default, def)
import Data.Lens (view, (.~), (^.))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (class Newtype)
import Data.Traversable (sequence)
import Data.Triple (Triple(..))
import Data.UUID (genUUID)
import Editor.Common.Lenses (_edges, _position, _rightEdge, _slope, _vertices)
import Effect (Effect)
import Math.LineSeg (mkLineSeg)
import Model.Polygon (Polygon, normalizeContour, polyWindows)
import SmartHouse.Algorithm.Edge (Edge(..))
import SmartHouse.Algorithm.EdgeInfo (mkEdgeInfo)
import SmartHouse.Algorithm.VertInfo (VertInfo, VertWithSlope, _bisector, vertInfoFrom)
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
mkVi (Triple prev p next) = vertInfoFrom pp Nothing 0.0 leftE rightE Nothing Nothing
    where prevP = prev ^. _position
          pp    = p ^. _position
          nextP = next ^. _position

          leftE = mkEdgeInfo (mkLineSeg prevP pp) (prev ^. _slope)
          rightE = mkEdgeInfo (mkLineSeg pp nextP) (p ^. _slope)

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
        line          : lv ^. _rightEdge,
        leftVertex    : lp,
        rightVertex   : rp,
        leftBisector  : lv ^. _bisector,
        rightBisector : rv ^. _bisector,
        normal        : n
        }

houseParamFrom :: Polygon VertWithSlope -> Effect HouseParam
houseParamFrom poly = do
    let vis = mkVi <$> polyWindows (normalizeContour (view _position) poly)
        ns  = fromMaybe vis $ Arr.snoc <$> Arr.tail vis <*> Arr.head vis
    edges <- sequence $ zipWith edge vis ns

    pure $ def # _vertices .~ vis
               # _edges    .~ edges
