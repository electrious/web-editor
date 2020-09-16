module Test where

import Prelude

import API (_auth, _baseUrl)
import Control.Monad.Except (runExcept)
import Control.Plus (empty)
import Data.Default (def)
import Data.Either (Either(..))
import Data.Lens ((.~), (^.))
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Editor.Common.Lenses (_apiConfig, _houseId, _leadId, _modeDyn, _panelType, _panels, _textureInfo)
import Editor.Editor (_roofUpdate, createEditor, loadHouse)
import Editor.EditorM (_elem, _sizeDyn, runEditorM)
import Editor.EditorMode (EditorMode(..))
import Editor.HouseEditor (_dataServer, _roofPlates, runHouseEditor)
import Editor.SceneEvent (size)
import Effect (Effect)
import Effect.Class.Console (logShow)
import FRP.Dynamic (step)
import FRP.Event (subscribe)
import Foreign (Foreign)
import Foreign.Generic (decode)
import Model.Hardware.PanelTextureInfo (_premium, _standard, _standard72)
import Model.Hardware.PanelType (PanelType(..))
import Web.DOM.NonElementParentNode (getElementById)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toNonElementParentNode)
import Web.HTML.Window (document)


foreign import solarModuleJPG :: String
foreign import qCellSolarPanelJPG :: String
foreign import qCellSolarPanel72PNG :: String

serverUrl :: String
serverUrl = "http://data.electrious.com"

doTest :: Foreign -> Foreign -> Effect Unit
doTest roofDat panelDat = do
    w <- window
    doc <- document w
    elem <- getElementById "editor" (toNonElementParentNode doc)

    case runExcept $ decode roofDat of
        Left e -> logShow e
        Right roofs -> case runExcept $ decode panelDat of
            Left e -> logShow e
            Right panels -> do
                let modeDyn = step ArrayEditing empty
                    sizeDyn = step (size 800 600) empty
                    panelType = step Standard empty

                    textures = def # _standard   .~ Just solarModuleJPG
                                   # _premium    .~ Just qCellSolarPanelJPG
                                   # _standard72 .~ Just qCellSolarPanel72PNG
                    apiCfg = def # _auth .~ Just "Bearer eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJJZCI6IiIsImN0YyI6NCwianRpIjoiNCJ9.d6pG95A4EoAPGhhnN4BsL7QtarpBRCEcta0Uu72SoVU"
                                 # _baseUrl .~ "https://api.electrious.com/v1"

                    cfg = def # _elem       .~ elem
                              # _modeDyn    .~ modeDyn
                              # _sizeDyn    .~ sizeDyn
                    
                    houseCfg = def # _modeDyn     .~ modeDyn
                                   # _houseId     .~ 4
                                   # _leadId      .~ 296285
                                   # _roofPlates  .~ roofs
                                   # _panels      .~ panels
                                   # _dataServer  .~ serverUrl
                                   # _panelType   .~ panelType
                                   # _textureInfo .~ textures
                                   # _apiConfig   .~ apiCfg
                editor <- runEditorM createEditor cfg
                res <- traverse (flip runHouseEditor houseCfg <<< loadHouse) editor

                case res of
                    Just house -> void $ subscribe (house ^. _roofUpdate) logShow
                    Nothing -> pure unit

                pure unit