module Editor.Editor (createEditor) where

import Prelude hiding (add)

import Control.Monad.Reader (ask)
import Control.Plus (empty)
import Data.Lens ((^.))
import Data.Maybe (Maybe(..))
import Editor.Disposable (dispose)
import Editor.House (loadHouseModel)
import Editor.RoofManager (_editedRoofs, _roofWrapper, createRoofManager)
import Editor.WebEditor (WebEditor, _dataServer, _defSize, _elem, _leadId, _roofPlates, _sizeEvt, addDisposable, performEditorEvent)
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
            (EditorScene es) <- liftEffect $ createScene (cfg ^. _defSize)
                                                         (cfg ^. _sizeEvt)
                                                         elem

            -- start the rednerring
            liftEffect $ window >>= renderLoop es.render

            let f hmd = do
                    liftEffect $ es.addContent hmd.wrapper
                    mgr <- liftEffect $ createRoofManager hmd (cfg ^. _roofPlates)
                    liftEffect $ es.addContent (mgr ^. _roofWrapper)
                    addDisposable $ dispose mgr
                    pure (mgr ^. _editedRoofs)

            e <- liftEffect $ loadHouseModel (cfg ^. _dataServer) (cfg ^. _leadId)
            keepLatest <$> performEditorEvent (f <$> e)
