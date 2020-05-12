module Editor.Editor (createEditor, loadHouse) where

import Prelude hiding (add)

import API.Racking (loadRacking)
import Control.Monad.Reader (ask)
import Data.Lens ((^.))
import Data.Maybe (Maybe)
import Data.Traversable (traverse)
import Editor.Common.Lenses (_houseId, _leadId, _modeDyn, _roofRackings, _wrapper)
import Editor.Disposable (dispose)
import Editor.EditorM (EditorM, _elem, _sizeDyn)
import Editor.House (loadHouseModel)
import Editor.HouseEditor (HouseEditor, _dataServer, performEditorEvent, runAPIInEditor)
import Editor.RoofManager (_editedRoofs, createRoofManager)
import Editor.WebEditor (WebEditor, addDisposable, addToScene, createScene, renderLoop)
import Effect.Class (liftEffect)
import FRP.Event (Event, keepLatest)
import Model.Roof.RoofPlate (RoofEdited)
import Unsafe.Coerce (unsafeCoerce)
import Web.HTML (window)

-- | createEditor will create the Web Editor instance
createEditor :: forall a. EditorM (Maybe (WebEditor a))
createEditor = do
    cfg <- ask

    let mkEditor elem = do
            scene <- liftEffect $ createScene (cfg ^. _sizeDyn)
                                              (cfg ^. _modeDyn)
                                              elem
            -- start the rednerring
            liftEffect $ window >>= renderLoop scene
            pure scene
    traverse mkEditor (cfg ^. _elem)


loadHouse :: forall a. WebEditor a -> HouseEditor (Event (Array RoofEdited))
loadHouse editor = do
    cfg <- ask

    let f hmd rackSys = do
            liftEffect $ addToScene (unsafeCoerce $ hmd ^. _wrapper) editor
            mgr <- createRoofManager hmd (rackSys ^. _roofRackings)
            liftEffect $ addToScene (mgr ^. _wrapper) editor
            liftEffect $ addDisposable (dispose mgr) editor
            pure (mgr ^. _editedRoofs)

    e <- liftEffect $ loadHouseModel (cfg ^. _dataServer) (cfg ^. _leadId)
    racksEvt <- runAPIInEditor $ loadRacking (cfg ^. _houseId)

    keepLatest <$> performEditorEvent (f <$> e <*> racksEvt)
