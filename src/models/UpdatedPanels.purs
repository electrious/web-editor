module Model.UpdatedPanels where

import Prelude

import Data.Default (class Default)
import Data.Foldable (class Foldable)
import Data.Lens ((%~), (^.))
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Map (Map, difference, insert, lookup, union)
import Data.Map as Map
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Data.Tuple (Tuple(..), snd)
import Data.UUIDWrapper (UUID)
import Data.Unfoldable (class Unfoldable)
import Model.Roof.Panel (Panel, _uuid)

newtype UpdatedPanels = UpdatedPanels (Map UUID Panel)
derive instance newtypeUpdatedPanels :: Newtype UpdatedPanels _
instance defaultUpdatedPanels :: Default UpdatedPanels where
    def = empty

empty :: UpdatedPanels
empty = UpdatedPanels Map.empty

isEmpty :: UpdatedPanels -> Boolean
isEmpty (UpdatedPanels m) = Map.isEmpty m

toUnfoldable :: forall f. Unfoldable f => Functor f => UpdatedPanels -> f Panel
toUnfoldable (UpdatedPanels m) = snd <$> Map.toUnfoldable m

fromFoldable :: forall f. Foldable f => Functor f => f Panel -> UpdatedPanels
fromFoldable = UpdatedPanels <<< Map.fromFoldable <<< map mkT
    where mkT p = Tuple (p ^. _uuid) p

get :: UUID -> UpdatedPanels -> Maybe Panel
get pid (UpdatedPanels m) = lookup pid m

add :: Panel -> UpdatedPanels -> UpdatedPanels
add p u = u # _Newtype %~ insert (p ^. _uuid) p

addPanels :: forall f. Foldable f => Functor f => f Panel -> UpdatedPanels -> UpdatedPanels
addPanels ps = merge (fromFoldable ps)

delete :: UUID -> UpdatedPanels -> UpdatedPanels
delete pid u = u # _Newtype %~ Map.delete pid

deletePanels :: forall f. Foldable f => Functor f => f UUID -> UpdatedPanels -> UpdatedPanels
deletePanels pids u = u # _Newtype %~ flip difference (Map.fromFoldable $ mkT <$> pids)
    where mkT pid = Tuple pid unit

merge :: UpdatedPanels -> UpdatedPanels -> UpdatedPanels
merge (UpdatedPanels u1) (UpdatedPanels u2) = UpdatedPanels $ union u1 u2
