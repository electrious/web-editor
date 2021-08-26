module SmartHouse.ChimneyBuilder where

import Prelude

import Control.Monad.Reader (ask)
import Control.Plus (empty)
import Data.Default (def)
import Data.Lens (view, (.~), (^.))
import Data.Maybe (Maybe(..))
import Data.Meter (Meter)
import Editor.Common.Lenses (_face, _height, _name, _parent, _point, _position, _scale, _tapped)
import Editor.ObjectAdder (AdderType(..), createObjectAdder, mkCandidatePoint)
import Editor.SceneEvent (SceneMouseMoveEvent)
import Effect.Unsafe (unsafePerformEffect)
import FRP.Dynamic (Dynamic, distinctDyn, step)
import FRP.Event (Event)
import FRP.Event.Extra (performEvent)
import Model.ActiveMode (ActiveMode, isActive)
import Model.SmartHouse.Chimney (Chimney, ChimneyNode, chimneyScale, mkChimney)
import Model.UUID (idLens)
import Rendering.Node (Node, _visible, fixNodeDWith, node, tapMesh)
import Three.Core.Face3 (normal)
import Three.Core.Geometry (BoxGeometry, mkBoxGeometry)
import Three.Core.Material (MeshPhongMaterial, mkMeshPhongMaterial)
import Three.Core.Object3D (worldToLocal)
import Three.Math.Vector (Vector3, mkVec3)


addChimney :: forall e. Dynamic ActiveMode -> Event SceneMouseMoveEvent -> Node e (Event Chimney)
addChimney actDyn mouseEvt = node (def # _name .~ "chimney-builder"
                                       # _visible .~ (isActive <$> actDyn)) do
    e <- ask
    let parent = e ^. _parent

        -- get candidate point
        getCandPoint evt = do
            np <- worldToLocal (evt ^. _point) parent
            pure $ Just $ mkCandidatePoint np (normal $ evt ^. _face)
       
        candPntDyn = step Nothing $ performEvent $ getCandPoint <$> mouseEvt

        opt = def # _name .~ "chimney-adder"
                  # _position .~ pure (mkVec3 0.0 0.0 0.01)
        
    addedPntEvt <- node opt $ createObjectAdder DefaultAdder candPntDyn (isActive <$> actDyn)

    pure $ performEvent $ mkChimney <<< view _position <$> addedPntEvt


chimGeo :: BoxGeometry
chimGeo = unsafePerformEffect $ mkBoxGeometry 1.0 1.0 1.0

chimMat :: MeshPhongMaterial
chimMat = unsafePerformEffect $ mkMeshPhongMaterial 0xb35900

editChimney :: forall e. Chimney -> Dynamic ActiveMode -> Node e ChimneyNode
editChimney chimney actDyn = fixNodeDWith chimney $ \chimDyn -> do
    let posDyn   = distinctDyn $ view _position <$> chimDyn
        scaleDyn = distinctDyn $ chimneyScale <$> chimDyn

    tapEvt <- view _tapped <$> tapMesh (def # _name .~ "tap"
                                            # _position .~ posDyn
                                            # _scale .~ scaleDyn
                                        ) chimGeo chimMat
    
    let chimneyEvt = empty

        cid = chimney ^. idLens
        cn = def # idLens .~ cid
                 # _tapped .~ (const cid <$> tapEvt)
    pure { input: chimneyEvt, output: cn }


heightBtn :: forall e. Dynamic Boolean -> Chimney -> Dynamic Vector3 -> Node e (Event Meter)
heightBtn actDyn chimney posDyn = node (def # _name .~ "height-btn"
                                            # _position .~ posDyn) $
    fixNodeDWith (chimney ^. _height) \heightDyn -> do
        let h = chimney ^. _height

            cfg = def # _height .~ h
        
        hEvt <- view _height <$> buildDragBtn actDyn cfg
        pure { input : hEvt, output: hEvt }
