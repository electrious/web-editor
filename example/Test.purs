module Test where

import Prelude

import API (_auth, _baseUrl)
import Control.Monad.Except (runExcept)
import Data.Default (def)
import Data.Either (Either(..))
import Data.Lens ((.~), (^.))
import Data.Maybe (Maybe(..))
import Editor.Common.Lenses (_apiConfig, _houseId, _leadId, _panelType, _panels, _textureInfo)
import Editor.Editor (_sizeDyn, createEditor)
import Editor.EditorMode (EditorMode(..))
import Editor.HouseEditor (_dataServer, _heatmapTexture, _roofplates, _rotBtnTexture)
import Editor.HouseLoader (editHouse)
import Editor.SceneEvent (size)
import Effect (Effect)
import Effect.Class.Console (logShow)
import FRP.Event (subscribe)
import FRP.Event.Extra (delay)
import Foreign (Foreign)
import Foreign.Generic (decode)
import Model.Hardware.PanelTextureInfo (_premium, _standard, _standard72)
import Model.Hardware.PanelType (PanelType(..))
import Model.Roof.Panel (Panel)
import Model.Roof.RoofPlate (RoofPlate)
import UI.RoofEditorUI (_editorOp)
import Web.DOM.NonElementParentNode (getElementById)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toNonElementParentNode)
import Web.HTML.Window (document)


foreign import solarModuleJPG :: String
foreign import qCellSolarPanelJPG :: String
foreign import qCellSolarPanel72PNG :: String
foreign import rotateButtonPNG :: String
foreign import heatmapGradientPNG :: String

serverUrl :: String
serverUrl = "http://data.electrious.com"

doTest :: Foreign -> Foreign -> Effect Unit
doTest roofDat panelDat = do
    w <- window
    doc <- document w
    elem <- getElementById "editor" (toNonElementParentNode doc)

    case runExcept $ decode roofDat of
        Left e -> logShow e
        Right (roofs :: Array RoofPlate) -> case runExcept $ decode panelDat of
            Left e -> logShow e
            Right (panels :: Array Panel) -> case elem of
                Nothing -> logShow "can't find 'editor' element"
                Just el -> do
                    let sizeDyn   = pure (size 1000 800)
                        panelType = pure Standard

                        textures = def # _standard   .~ Just solarModuleJPG
                                       # _premium    .~ Just qCellSolarPanelJPG
                                       # _standard72 .~ Just qCellSolarPanel72PNG
                        apiCfg = def # _auth    .~ Just "Bearer eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJJZCI6IiIsImN0YyI6NCwianRpIjoiNCJ9.d6pG95A4EoAPGhhnN4BsL7QtarpBRCEcta0Uu72SoVU"
                                     # _baseUrl .~ "https://api.electrious.com/v1"

                        houseCfg = def # _houseId        .~ 4
                                       # _leadId         .~ 296285
                                       # _roofplates     .~ roofs
                                       # _panels         .~ panels
                                       # _dataServer     .~ serverUrl
                                       # _panelType      .~ panelType
                                       # _textureInfo    .~ textures
                                       # _rotBtnTexture  .~ rotateButtonPNG
                                       # _heatmapTexture .~ heatmapGradientPNG
                                       # _apiConfig      .~ apiCfg

                    editor <- createEditor el $ def # _sizeDyn .~ sizeDyn

                    house <- editHouse editor houseCfg (delay 10 $ pure RoofEditing)

                    void $ subscribe (house ^. _editorOp) logShow
                    --void $ subscribe (house ^. _screenshot) logShow

                    {-
                    let builderCfg = def # _leadId   .~ 318872
                                         
                    r <- buildHouse editor builderCfg

                    let readyEvt = const unit <$> dynEvent (r ^. _houseReady)
                    void $ subscribe (r ^. _filesExported) logShow
                    pure unit -}
