module HouseBuilder.RidgeBuilder where


import Prelude

import Control.Plus (empty)
import Data.Default (def)
import Data.Lens (view, (.~), (^.))
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Editor.Common.Lenses (_face, _mouseMove, _name, _point, _position)
import Editor.ObjectAdder (_addedPoint, createObjectAdder, mkCandidatePoint)
import Editor.SceneEvent (SceneMouseMoveEvent)
import FRP.Dynamic (Dynamic, gateDyn, step)
import FRP.Event (Event)
import FRP.Event.Extra (performEvent)
import Model.ActiveMode (ActiveMode)
import Model.HouseBuilder.FloorPlan (FloorPlan)
import Model.HouseEditor.HousePoint (RidgePoint, ridgePoint)
import Rendering.Node (Node, getParent, node)
import Three.Core.Face3 (normal)
import Three.Core.Object3D (worldToLocal)
import Three.Math.Vector (mkVec3)


addRidgePoint :: forall e. RidgeEditorConf -> Dynamic Boolean -> Node e (Event RidgePoint)
addRidgePoint cfg canShowDyn = do
    parent <- getParent

    -- get a candidate point
    let getCandPoint evt = do
            np <- worldToLocal (evt ^. _point) parent
            pure $ Just $ mkCandidatePoint np (normal $ evt ^. _face)

        candPntDyn = step Nothing $ performEvent $ getCandPoint <$> gateDyn canShowDyn (cfg ^. _mouseMove)

        opt = def # _name .~ "ridge-point-adder"
                  # _position .~ pure (mkVec3 0.0 0.0 0.1)
    adder <- node opt $ createObjectAdder candPntDyn canShowDyn
    
    pure $ ridgePoint <<< view _position <$> adder ^. _addedPoint

newtype RidgeEditorConf = RidgeEditorConf {
    floor     :: FloorPlan,
    mode      :: Dynamic ActiveMode,
    mouseMove :: Event SceneMouseMoveEvent
    }

derive instance newtypeRidgeEditorConf :: Newtype RidgeEditorConf _

editRidges :: forall e. RidgeEditorConf -> Node e (Event Unit)
editRidges conf = do
    
    pure empty
