module HouseBuilder.RoofSurfaceBuilder where

import Prelude

import Control.Plus (empty)
import Data.Default (class Default, def)
import Data.Lens (Lens', (.~), (^.))
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.List (List)
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy(..))
import Editor.Common.Lenses (_polygon)
import Editor.PolygonEditor (_delete, createPolyEditor)
import FRP.Event (Event)
import Model.Hardware.PanelModel (_isActive)
import Model.HouseBuilder.RoofSurface (RoofSurface, newSurface)
import Rendering.Node (Node)


newtype RoofSurfEditor = RoofSurfEditor {
    surface :: Event RoofSurface,
    delete  :: Event Unit
    }

derive instance newtypeRoofSurfEditor :: Newtype RoofSurfEditor _
instance defaultRoofSurfEditor :: Default RoofSurfEditor where
    def = RoofSurfEditor {
        surface : empty,
        delete  : empty
        }

_surface :: forall t a r. Newtype t { surface :: a | r } => Lens' t a
_surface = _Newtype <<< prop (SProxy :: SProxy "surface")

editRoofSurface :: forall e. RoofSurface -> Node e RoofSurfEditor
editRoofSurface rs = do
    let cfg = def # _isActive .~ pure true
                  # _polygon  .~ rs ^. _polygon
                  
    editor <- createPolyEditor cfg
    
    pure $ def # _surface .~ (newSurface <$> editor ^. _polygon)
               # _delete  .~ (const unit <$> editor ^. _delete)


newtype BuilderState = BuilderState {
    surfaces :: List RoofSurface
    }

derive instance newtypeBuilderState :: Newtype BuilderState _


_surfaces :: forall t a r. Newtype t { surfaces :: a | r } => Lens' t a
_surfaces = _Newtype <<< prop (SProxy :: SProxy "surfaces")
