module HouseBuilder.RidgeBuilder where


import Prelude

import Control.Plus (empty)
import Data.Default (class Default, def)
import Data.Lens (Lens', view, (%~), (.~), (^.))
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.List (List(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy(..))
import Editor.Common.Lenses (_face, _mouseMove, _name, _point, _position)
import Editor.ObjectAdder (createObjectAdder, mkCandidatePoint)
import Editor.SceneEvent (SceneMouseMoveEvent)
import FRP.Dynamic (Dynamic, gateDyn, step)
import FRP.Event (Event)
import FRP.Event.Extra (performEvent)
import Model.ActiveMode (ActiveMode)
import Model.HouseBuilder.FloorPlan (FloorPlan)
import Model.HouseBuilder.House (_ridges)
import Model.HouseBuilder.Ridge (Ridge, topRidge)
import Model.HouseEditor.HousePoint (RidgePoint, ridgePoint)
import Rendering.Node (Node, getParent, node)
import Three.Core.Face3 (normal)
import Three.Core.Object3D (worldToLocal)
import Three.Math.Vector (mkVec3)


-- | let user add new ridge point
addRidgePoint :: forall e. RidgeEditorConf -> Dynamic Boolean -> Node e (Event RidgePoint)
addRidgePoint cfg canShowDyn = do
    parent <- getParent

    -- get a candidate point
    let getCandPoint evt = do
            np <- worldToLocal (evt ^. _point) parent
            pure $ Just $ mkCandidatePoint np (normal $ evt ^. _face)

        pntsEvt      = performEvent $ getCandPoint <$> gateDyn canShowDyn (cfg ^. _mouseMove)
        candPntDyn   = step Nothing pntsEvt

        toRidgePoint = ridgePoint <<< view _position
        
        opt = def # _name     .~ "ridge-point-adder"
                  # _position .~ pure (mkVec3 0.0 0.0 0.1)
                  
    map toRidgePoint <$> node opt (createObjectAdder candPntDyn canShowDyn)


newtype RidgeEditorState = RidgeEditorState {
    ridgePoints :: List RidgePoint,
    ridges      :: List Ridge,
    tempRidge   :: Maybe Ridge
    }

derive instance newtypeRidgeEditorState :: Newtype RidgeEditorState _
instance defaultRidgeEditorState :: Default RidgeEditorState where
    def = RidgeEditorState {
        ridgePoints : Nil,
        ridges      : Nil,
        tempRidge   : Nothing
        }

_ridgePoints :: forall t a r. Newtype t { ridgePoints :: a | r } => Lens' t a
_ridgePoints = _Newtype <<< prop (SProxy :: SProxy "ridgePoints")

_tempRidge :: forall t a r. Newtype t { tempRidge :: a | r } => Lens' t a
_tempRidge = _Newtype <<< prop (SProxy :: SProxy "tempRidge")

data RidgeOp = ROAddRidgePoint RidgePoint
             | RODragRidgePoint RidgePoint RidgePoint
             | ROAddRidge Ridge


applyOp :: RidgeOp -> RidgeEditorState -> RidgeEditorState
applyOp (ROAddRidgePoint p)     st = st # _ridgePoints %~ Cons p
applyOp (RODragRidgePoint p np) st = st # _tempRidge .~ Just (topRidge p np)
applyOp (ROAddRidge r)          st = st # _ridges %~ Cons r
                                        # _tempRidge .~ Nothing


newtype RidgeEditorConf = RidgeEditorConf {
    floor     :: FloorPlan,
    mode      :: Dynamic ActiveMode,
    mouseMove :: Event SceneMouseMoveEvent
    }

derive instance newtypeRidgeEditorConf :: Newtype RidgeEditorConf _

editRidges :: forall e. RidgeEditorConf -> Node e (Event Unit)
editRidges conf = do
    
    pure empty
