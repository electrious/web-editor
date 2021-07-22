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
import Editor.Common.Lenses (_edges, _position, _slope, _vertices)
import Effect (Effect)
import Math.LineSeg (mkLineSeg)
import Model.Polygon (Polygon, normalizeContour, polyWindows)
import SmartHouse.Algorithm.Edge (Edge, edge)
import SmartHouse.Algorithm.EdgeInfo (mkEdgeInfo)
import SmartHouse.Algorithm.VertInfo (VertInfo, VertWithSlope, vertInfoFrom)

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
mkVi (Triple prev p next) = vertInfoFrom pp 0.0 leftE rightE Nothing Nothing
    where prevP = prev ^. _position
          pp    = p ^. _position
          nextP = next ^. _position

          leftE = mkEdgeInfo (mkLineSeg prevP pp) (prev ^. _slope)
          rightE = mkEdgeInfo (mkLineSeg pp nextP) (p ^. _slope)

houseParamFrom :: Polygon VertWithSlope -> Effect HouseParam
houseParamFrom poly = do
    let vis = mkVi <$> polyWindows (normalizeContour (view _position) poly)
        ns  = fromMaybe vis $ Arr.snoc <$> Arr.tail vis <*> Arr.head vis
    edges <- sequence $ zipWith edge vis ns

    pure $ def # _vertices .~ vis
               # _edges    .~ edges
