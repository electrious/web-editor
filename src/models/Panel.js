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

exports.getUUID = p => {
    return p.getUuid()
}

exports.setUUID = u => p => _ => {
    p.setUuid(u)
}

exports.getLeadId = p => {
    return p.getLeadId()
}

exports.setLeadId = i => p => _ => {
    p.setLeadId(i)
}

exports.getRoofplateUUID = p => {
    return p.getRoofplateUuid()
}

exports.setRoofplateUUID = u => p => _ => {
    p.setRoofplateUuid(u)
}

exports.getRowNumber = p => {
    return p.getRowNumber()
}

exports.setRowNumber = r => p => _ => {
    p.setRowNumber(r)
}

exports.getSlope = p => {
    return p.getSlope()
}

exports.setSlope = s => p => _ => {
    p.setSlope(s)
}

exports.getOrientation = p => {
    return p.getOrientation()
}

exports.setOrientation = o => p => _ => {
    p.setOrientation(o)
}

exports.getAlignment = p => {
    return p.getAlignment()
}

exports.setAlignment = a => p => _ => {
    p.setAlignment(a)
}