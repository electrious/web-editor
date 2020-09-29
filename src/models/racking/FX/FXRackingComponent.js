const { RailFreeComponent } = require('webpb/electrious/models/racking/component_pb')

exports.mkRailFreeComponentPB = _ => {
    return new RailFreeComponent()
}
