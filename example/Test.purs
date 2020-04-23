module Test where

import Prelude

import Data.Maybe (Maybe(..))
import Editor.Editor (createEditor)
import Effect (Effect)
import Effect.Class.Console (logShow)
import FRP.Event (subscribe)
import Models.RoofPlate (JSRoofPlate)
import Web.DOM.NonElementParentNode (getElementById)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toNonElementParentNode)
import Web.HTML.Window (document)

serverUrl :: String
serverUrl = "https://s3.eu-west-1.amazonaws.com/data.electrious.com"

doTest :: Array JSRoofPlate -> Effect Unit
doTest roofs = do
    w <- window
    doc <- document w
    elem <- getElementById "editor" (toNonElementParentNode doc)

    case elem of
        Nothing -> pure unit
        Just e -> do
            editor <- createEditor 800 600 e
            res <- editor.loadHouse serverUrl 296285 roofs
            
            _ <- subscribe res logShow
            pure unit