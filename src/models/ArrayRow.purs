module Model.ArrayRow where

import Prelude

import Data.Foldable (class Foldable)
import Data.Lens ((^.))
import Data.Map (Map, fromFoldable)
import Data.Newtype (class Newtype)
import Data.Tuple (Tuple(..))
import Editor.Common.Lenses (_rowNumber)
import Model.Roof.Panel (Orientation, Panel)

newtype ArrayRow = ArrayRow {
    rowNumber   :: Int,
    orientation :: Orientation,
    panels      :: Array Panel
}
derive instance newtypeArrayRow :: Newtype ArrayRow _

rowsToMap :: forall f. Foldable f => Functor f => f ArrayRow -> Map Int ArrayRow
rowsToMap = fromFoldable <<< map mkT
    where mkT r = Tuple (r ^. _rowNumber) r
