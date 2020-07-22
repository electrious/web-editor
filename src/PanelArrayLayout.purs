module Editor.PanelArrayLayout where

import Prelude

import Data.Array as Array
import Data.Filterable (filter)
import Data.FoldableWithIndex (foldlWithIndex)
import Data.Graph (Graph, connectedComponents, fromAdjacencyList, vertices)
import Data.Lens (Lens', view, (^.))
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.List (List(..), concat, concatMap, foldl, fromFoldable, mapWithIndex, sortBy)
import Data.Map (Map, lookup, values)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Meter (meterVal)
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple(..))
import Data.Unfoldable as Unfoldable
import Editor.Common.Lenses (_centerX, _centerY, _config, _panels, _panelsUpdated)
import Effect (Effect)
import Global (infinity)
import Math (abs)
import Model.PanelArray (PanelArray, mkArrayWithCenter, mkPanelArray, mkPanelArrayMap)
import Model.Roof.ArrayConfig (ArrayConfig, _gapY)
import Model.Roof.Panel (Orientation, Panel, validatedSlope)
import Model.RoofComponent (compBBox, compBBoxWithOffset)
import Model.UpdatedPanels (UpdatedPanels, empty, merge)
import RBush.RBush (RBush, load, mkRBush, search)

-- | a helper data type for PanelLayer to manage the internal RTree and graph of panels
newtype PanelsLayout = PanelsLayout {
    -- all panels on this panel layer (roof) is stored and managed in
    -- an RTree and a list of panels
    tree          :: RBush Panel,
    panels        :: List Panel,

    config        :: ArrayConfig,
    arrays        :: Map Int PanelArray,
    panelsUpdated :: UpdatedPanels
}

derive instance newtypePanelsLayout :: Newtype PanelsLayout _

_tree :: Lens' PanelsLayout (RBush Panel)
_tree = _Newtype <<< prop (SProxy :: SProxy "tree")

_arrays :: Lens' PanelsLayout (Map Int PanelArray)
_arrays = _Newtype <<< prop (SProxy :: SProxy "arrays")

layoutPanels :: Array Panel -> ArrayConfig -> Effect PanelsLayout
layoutPanels ps cfg = do
    tree <- mkRTree ps
    let graph              = mkGraph ps tree cfg
        (Tuple arrs toUpd) = splitPanelArrays graph cfg
        panels             = concatMap (view _panels) $ values arrs
    newTree <- mkRTree $ Array.fromFoldable panels

    pure $ PanelsLayout {
        tree          : newTree,
        panels        : panels,
        config        : cfg,
        arrays        : arrs,
        panelsUpdated : toUpd
    }

defaultLayout :: Orientation -> ArrayConfig -> Effect PanelsLayout
defaultLayout o cfg = do
    t <- mkRTree []
    pure $ PanelsLayout {
        tree          : t,
        panels        : Nil,
        config        : cfg,
        arrays        : Map.empty,
        panelsUpdated : empty
    }

neighbors :: PanelsLayout -> Panel -> List Panel
neighbors l p = neighborPanelsInTree p (l ^. _tree) (l ^. _config)

getArrayAt :: Int -> PanelsLayout -> Maybe PanelArray
getArrayAt k = lookup k <<< view _arrays

getPanelsInArray :: Int -> PanelsLayout -> List Panel
getPanelsInArray arr = concat <<< Unfoldable.fromMaybe <<< map (view _panels) <<< getArrayAt arr

type AccItem = {
    arr :: Maybe Int,
    x :: Number,
    y :: Number
}

findActiveArray :: PanelsLayout -> Int
findActiveArray l = fromMaybe 0 $ (_.arr) $ foldlWithIndex f init (l ^. _arrays)
    where init = { arr: Nothing, x: infinity, y: infinity }
          f i res arr = let cx = meterVal (arr ^. _centerX)
                            cy = meterVal (arr ^. _centerY)
                        in if (abs (cy - res.y) < 0.01 && cx < res.x) || cy < res.y
                           then { arr: Just i, x: cx, y: cy }
                           else res

mkRTree :: Array Panel -> Effect (RBush Panel)
mkRTree ps = do
    t <- mkRBush
    let getBox p = compBBox p (validatedSlope p) 
    load (getBox <$> ps) t
    pure t

neighborPanelsInTree :: Panel -> RBush Panel -> ArrayConfig -> List Panel
neighborPanelsInTree p tree cfg = filter ((/=) p) $ fromFoldable $ search box tree
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
    let comp arr1 arr2 = let x1 = arr1 ^. _centerX
                             y1 = arr1 ^. _centerY
                             x2 = arr2 ^. _centerX
                             y2 = arr2 ^. _centerY
                          in if (x1 < x2 && y1 < y2) || not (x1 > x2 && y1 > y2) || y1 <= y2
                             then LT
                             else GT
        pArrs = sortBy comp $ (mkArrayWithCenter <<< vertices) <$> connectedComponents graph

        mkArr arrNum arrC = mkPanelArray arrNum arrC cfg
        arrLst = mapWithIndex mkArr pArrs
        upd    = foldl merge empty $ view _panelsUpdated <$> arrLst
    in Tuple (mkPanelArrayMap arrLst) upd
