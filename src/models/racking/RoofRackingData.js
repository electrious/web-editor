const {
    Component
} = require('webpb/electrious/models/racking/component_pb')

exports.mkComponentPB = _ => {
    return new Component()
}

exports.rdTypeNotSet = Component.RdTypeCase.RD_TYPE_NOT_SET
exports.rdTypeRail = Component.RdTypeCase.RAIL
exports.rdTypeRailFree = Component.RdTypeCase.RAIL_FREE
exports.rdTypeRailFlat = Component.RdTypeCase.RAIL_FLAT
exports.rdTypeBallast = Component.RdTypeCase.BALLAST
exports.rdTypeGAF = Component.RdTypeCase.GAF

const {
    RoofRackingResult
} = require('webpb/electrious/models/racking_pb')

exports.mkRoofRackingResultPB = _ => {
    return new RoofRackingResult()
}
