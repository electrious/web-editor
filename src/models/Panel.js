const { Panel } = require("webpb/electrious/models/roofplate/panel_pb")

exports.mkPanelPB = _ => {
    return new Panel()
}

exports.orientationInvalid = Panel.Orientation.ORIENTATION_INVALID
exports.orientationLandscape = Panel.Orientation.ORIENTATION_LANDSCAPE
exports.orientationPortrait = Panel.Orientation.ORIENTATION_PORTRAIT

exports.alignmentInvalid = Panel.Alignment.ALIGNMENT_INVALID
exports.alignmentGrid = Panel.Alignment.ALIGNMENT_GRID
exports.alignmentBrick = Panel.Alignment.ALIGNMENT_BRICK
