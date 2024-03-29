module Test where

import Prelude

import API (_auth, _baseUrl)
import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (decodeJson)
import Data.Default (def)
import Data.Either (Either(..))
import Data.Filterable (filter)
import Data.Lens ((.~), (^.))
import Data.Maybe (Maybe(..))
import Editor.Common.Lenses (_apiConfig, _houseId, _leadId, _panelType, _panels, _textureInfo)
import Editor.Editor (_sizeDyn, createEditor)
import Editor.EditorMode (EditorMode(..))
import Editor.HouseEditor (_dataServer, _heatmapTexture, _roofplates, _rotBtnTexture)
import Editor.HouseLoader (editHouse)
import Taihe.SceneEvent (size)
import Effect (Effect)
import Effect.Class.Console (logShow)
import FRP.Event (subscribe)
import FRP.Event.Extra (delay)
import Model.Hardware.PanelTextureInfo (_premium, _standard, _standard72)
import Model.Hardware.PanelType (PanelType(..))
import Model.Roof.Panel (Panel)
import Model.Roof.RoofPlate (RoofPlate)
import SmartHouse.HouseBuilder (_hasHouse, buildHouse)
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

doTest :: Json -> Json -> Effect Unit
doTest roofDat panelDat = do
    w <- window
    doc <- document w
    elem <- getElementById "editor" (toNonElementParentNode doc)

    case decodeJson roofDat of
        Left e -> logShow e
        Right (roofs :: Array RoofPlate) -> case decodeJson panelDat of
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
                                     # _baseUrl .~ "https://api.electrious.com"

                        houseCfg = def # _houseId        .~ 4
                                       # _leadId         .~ 296285
                                       # _roofplates     .~ roofs
                                       # _panels         .~ panels
                                       # _dataServer     .~ serverUrl
                                       # _panelType      .~ panelType
                                       # _textureInfo    .~ textures
                                       # _rotBtnTexture  .~ rotateButtonPNG
                                       # _heatmapTexture .~ heatmapGradientPNG
                                       # _apiConfig      .~ pure apiCfg

                    editor <- createEditor el $ def # _sizeDyn .~ sizeDyn

                    let mode = HouseBuilding

                    if mode /= HouseBuilding
                       then do
                          house <- editHouse editor houseCfg (delay 10 $ pure mode)

                          void $ subscribe (house ^. _editorOp) logShow
                          --void $ subscribe (house ^. _screenshot) logShow

                        else do
                            let builderCfg = def # _leadId         .~ 359617   --318872
                                                 # _apiConfig      .~ pure apiCfg
                                                 # _dataServer     .~ serverUrl
                                                 # _textureInfo    .~ textures
                                                 # _rotBtnTexture  .~ rotateButtonPNG
                                                 # _heatmapTexture .~ heatmapGradientPNG
                                                 # _panelType      .~ panelType
                            r <- buildHouse editor builderCfg

                            let _ = const unit <$> filter identity (r ^. _hasHouse)
                            
                            pure unit
