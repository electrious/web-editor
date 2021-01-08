module HouseBuilder.RoofSurfaceBuilder where

import Prelude

import Control.Plus (empty)
import Data.Default (class Default, def)
import Data.Lens (Lens', view, (.~), (^.))
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.List (List(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy(..))
import Data.Traversable (traverse)
import Editor.Common.Lenses (_face, _mouseMove, _name, _point, _polygon, _position)
import Editor.ObjectAdder (createObjectAdder, mkCandidatePoint)
import Editor.PolygonEditor (_delete, createPolyEditor)
import Editor.SceneEvent (SceneMouseMoveEvent)
import FRP.Dynamic (Dynamic, gateDyn, step)
import FRP.Event (Event)
import FRP.Event.Extra (performEvent)
import Model.ActiveMode (ActiveMode)
import Model.Hardware.PanelModel (_isActive)
import Model.HouseBuilder.FloorPlan (FloorPlan)
import Model.HouseBuilder.RoofSurface (RoofSurface, newSurface, surfaceAround)
import Rendering.DynamicNode (eventNode)
import Rendering.Node (Node, fixNodeEWith, getParent, node)
import Three.Core.Face3 (normal)
import Three.Core.Object3D (worldToLocal)
import Three.Math.Vector (mkVec3)


-- | RoofSurface Editor to edit a single RoofSurface

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

-- | function to show an ObjectAdder to add a new roof surface
addSurface :: forall e. SurfaceBuilderCfg -> Dynamic Boolean -> Node e (Event RoofSurface)
addSurface cfg actDyn = do
    parent <- getParent

    -- get a candidate point
    let getCandPoint evt = do
            np <- worldToLocal (evt ^. _point) parent
            pure $ Just $ mkCandidatePoint np (normal $ evt ^. _face)

        pntsEvt = performEvent $ getCandPoint <$> gateDyn actDyn (cfg ^. _mouseMove)
        candPntDyn = step Nothing pntsEvt

        toSurf = surfaceAround <<< view _position

        opt = def # _name .~ "surface-adder"
                  # _position .~ pure (mkVec3 0.0 0.0 0.1)
    selPntEvt <- node opt (createObjectAdder candPntDyn actDyn)
    pure $ performEvent $ toSurf <$> selPntEvt


-- | Add/Remove/Update all roof surfaces

newtype SurfaceBuilderCfg = SurfaceBuilderCfg {
    floor     :: FloorPlan,
    mode      :: Dynamic ActiveMode,
    mouseMove :: Event SceneMouseMoveEvent
    }

derive instance newtypeSurfaceBuilderCfg :: Newtype SurfaceBuilderCfg _


newtype BuilderState = BuilderState {
    surfaces :: List RoofSurface
    }

derive instance newtypeBuilderState :: Newtype BuilderState _
instance defaultBuilderState :: Default BuilderState where
    def = BuilderState {
        surfaces : Nil
        }

_surfaces :: forall t a r. Newtype t { surfaces :: a | r } => Lens' t a
_surfaces = _Newtype <<< prop (SProxy :: SProxy "surfaces")

editSurfaces :: forall e. Dynamic Boolean -> Node e (Event (List RoofSurface))
editSurfaces actDyn = fixNodeEWith def \stEvt -> do
    let ssEvt = view _surfaces <$> stEvt

    ses <- eventNode $ traverse editRoofSurface <$> ssEvt

    pure { input : empty :: Event BuilderState, output : empty }
