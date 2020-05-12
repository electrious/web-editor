module Editor.Editor (createEditor) where

import Prelude hiding (add)

import API.Racking (loadRacking)
import Control.Monad.Reader (ask)
import Control.Plus (empty)
import Data.Lens ((^.))
import Data.Maybe (Maybe(..))
import Editor.Common.Lenses (_leadId, _houseId, _roofRackings, _wrapper)
import Editor.Disposable (dispose)
import Editor.House (loadHouseModel)
import Editor.RoofManager (_editedRoofs, createRoofManager)
import Editor.WebEditor (WebEditor, _dataServer, _elem, _modeDyn, _sizeDyn, addDisposable, performEditorEvent, runAPIInEditor)
import Editor.WebEditorScene (EditorScene(..), createScene, renderLoop)
import Effect.Class (liftEffect)
import Effect.Class.Console (errorShow)
import FRP.Event (Event, keepLatest)
import Model.Roof.RoofPlate (RoofEdited)
import Web.HTML (window)

-- | createEditor will create the Web Editor instance
createEditor :: WebEditor (Event (Array RoofEdited))
createEditor = do
    cfg <- ask

    case cfg ^. _elem of
        Nothing -> errorShow "elem is not set in EditorConfig" *> pure empty
        Just elem -> do
            (EditorScene es) <- liftEffect $ createScene (cfg ^. _sizeDyn)
                                                         (cfg ^. _modeDyn)
                                                         elem

            -- start the rednerring
            liftEffect $ window >>= renderLoop es.render

            let f hmd rackSys = do
                    liftEffect $ es.addContent $ hmd ^. _wrapper
                    mgr <- createRoofManager hmd (rackSys ^. _roofRackings)
                    liftEffect $ es.addContent (mgr ^. _wrapper)
                    addDisposable $ dispose mgr
                    pure (mgr ^. _editedRoofs)

            e <- liftEffect $ loadHouseModel (cfg ^. _dataServer) (cfg ^. _leadId)
            racksEvt <- runAPIInEditor $ loadRacking (cfg ^. _houseId)

            keepLatest <$> performEditorEvent (f <$> e <*> racksEvt)
