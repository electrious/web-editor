module UI.RoofEditorUI where

import Prelude

import Editor.EditorMode (EditorMode(..))
import Editor.HouseEditor (ArrayEditParam)
import Specular.Dom.Element (attr, attrs, class_, classes, el)
import Specular.Dom.Widget (Widget)
import Specular.FRP (Event, leftmost)
import UI.ArrayEditorUI (EditorPaneOpt, arrayEditorPane)
import UI.RoofInstructions (roofInstructions)
import UI.Utils (div, elA, mkStyle, (:~))

shadowStyle :: String
shadowStyle = "0 3px 1px -2px rgba(0, 0, 0, 0.2), 0 2px 2px 0 rgba(0, 0, 0, 0.14), 0 1px 5px 0 rgba(0, 0, 0, 0.06)"

roofEditorUI :: EditorPaneOpt -> Widget Unit
roofEditorUI opt =
    div [classes ["uk-overlay", "uk-overlay-default", "uk-padding-small", "uk-position-top-left"],
         attrs $ mkStyle ["box-shadow" :~ shadowStyle ]] do
        modeEvt  <- headerTab
        arrParam <- body opt

        pure unit

-- header tab of the pane switcher between Array and Roof editing
headerTab :: Widget (Event EditorMode)
headerTab = el "ul" [classes ["uk-subnav", "uk-subnav-pill"],
                     attr "uk-switcher" ""] do
    arrEvt  <- el "li" [] $ elA "Edit Arrays" "#"
    roofEvt <- el "li" [] $ elA "Edit Roofs" "#"

    pure $ leftmost [const ArrayEditing <$> arrEvt,
                     const RoofEditing <$> roofEvt]

body :: EditorPaneOpt -> Widget ArrayEditParam
body opt =
    el "ul" [class_ "uk-switcher"] do
        res <- el "li" [] $ arrayEditorPane opt
        el "li" [] roofInstructions
        pure res
