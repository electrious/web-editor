module Editor.PanelArrayLayout where

import Prelude

import Data.Array (filter)
import Data.Graph (Graph, connectedComponents, fromAdjacencyList, vertices)
import Data.Lens ((^.))
import Data.List (fromFoldable, sort)
import Data.Map (Map)
import Data.Meter (meterVal)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Model.PanelArray (PanelArray)
import Model.Roof.ArrayConfig (ArrayConfig, _gapY)
import Model.Roof.Panel (Panel, validatedSlope)
import Model.RoofComponent (compBBox, compBBoxWithOffset)
import Model.UpdatedPanels (UpdatedPanels(..), empty)
import RBush.RBush (RBush, load, mkRBush, search)

mkRTree :: Array Panel -> Effect (RBush Panel)
mkRTree ps = do
    t <- mkRBush
    let getBox p = compBBox p (validatedSlope p) 
    load (getBox <$> ps) t
    pure t

neighborPanelsInTree :: Panel -> RBush Panel -> ArrayConfig -> Array Panel
neighborPanelsInTree p tree cfg = filter ((/=) p) $ search box tree
    where box = compBBoxWithOffset p xOffset yOffset (validatedSlope p)
          xOffset = 0.4
          yOffset = 0.4 + meterVal (cfg ^. _gapY)

mkGraph :: Array Panel -> RBush Panel -> ArrayConfig -> Graph Panel Number
mkGraph ps tree cfg = fromAdjacencyList $ fromFoldable $ f <$> ps
    where f p = let ns = neighborPanelsInTree p tree cfg
                    mkNValue n = Tuple n 0.0
                in Tuple p $ mkNValue <$> fromFoldable ns

splitPanelArrays :: Graph Panel Number -> ArrayConfig -> Tuple (Map Int PanelArray) UpdatedPanels
splitPanelArrays graph cfg =
    let pArrs = vertices <$> connectedComponents graph
    in Tuple (toDict arrs) empty