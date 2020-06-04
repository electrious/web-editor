const { Component,
        RailComponent,
        RailFreeComponent,
        RailFlatComponent,
        BallastComponent,
        GAFComponent
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

exports.mkRailComponentPB = _ => {
    return new RailComponent()
}

exports.mkRailFreeComponentPB = _ => {
    return new RailFreeComponent()
}

exports.mkRailFlatComponentPB = _ => {
    return new RailFlatComponent()
}

exports.mkBallastComponentPB = _ => {
    return new BallastComponent()
}

exports.mkGAFComponentPB = _ => {
    return new GAFComponent()
}