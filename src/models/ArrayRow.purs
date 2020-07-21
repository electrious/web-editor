module Model.ArrayRow where

import Prelude

import Data.Foldable (class Foldable)
import Data.Lens ((^.))
import Data.List (List)
import Data.Map (Map, fromFoldable)
import Data.Newtype (class Newtype)
import Data.Tuple (Tuple(..))
import Model.Roof.Panel (Orientation, Panel)
import Editor.Common.Lenses (_rowNumber)

newtype ArrayRow = ArrayRow {
    rowNumber   :: Int,
    orientation :: Orientation,
    panels      :: List Panel
}
derive instance newtypeArrayRow :: Newtype ArrayRow _

mkArrayRow :: Int -> Orientation -> List Panel -> ArrayRow
mkArrayRow rowNum orient ps = ArrayRow {
    rowNumber   : rowNum,
    orientation : orient,
    panels      : ps
}

rowsToMap :: forall f. Foldable f => Functor f => f ArrayRow -> Map Int ArrayRow
rowsToMap = fromFoldable <<< map mkT
    where mkT r = Tuple (r ^. _rowNumber)r
