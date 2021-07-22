module SmartHouse.Algorithm.HouseParam where

import Prelude

import Data.Array (zipWith)
import Data.Array as Arr
import Data.Default (class Default, def)
import Data.Filterable (filter)
import Data.Lens ((.~))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (class Newtype)
import Data.Traversable (sequence)
import Data.Triple (Triple(..))
import Editor.Common.Lenses (_edges, _vertices)
import Effect (Effect)
import Math.LineSeg (mkLineSeg)
import Model.Polygon (Polygon, newPolygon, polyWindows)
import SmartHouse.Algorithm.Edge (Edge, edge)
import SmartHouse.Algorithm.VertInfo (VertInfo, vertInfoFrom)
import Three.Math.Vector (class Vector, getVector, normal, (<->))

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

-- delete duplicated vertices or connect two consecutive edges if they're in the same direction
normalizeContour :: forall v. Eq v => Vector v => Polygon v -> Polygon v
normalizeContour = newPolygon <<< map g <<< filter f <<< polyWindows
    where f (Triple prev p next) = not $ p == next || normal (p <-> prev) == normal (next <-> p)
          g (Triple _ p _) = p
          
houseParamFrom :: forall v. Eq v => Vector v => Polygon v -> Effect HouseParam
houseParamFrom poly = do
    let mkVi (Triple prev p next) = vertInfoFrom p 0.0 (mkLineSeg prev p) (mkLineSeg p next) Nothing Nothing
        vis = mkVi <$> polyWindows (getVector <$> normalizeContour poly)
        ns  = fromMaybe vis $ Arr.snoc <$> Arr.tail vis <*> Arr.head vis
    edges <- sequence $ zipWith edge vis ns

    pure $ def # _vertices .~ vis
               # _edges    .~ edges
