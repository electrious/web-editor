const { RailComponent } = require('webpb/electrious/models/racking/component_pb')

exports.mkRailComponentPB = _ => {
    return new RailComponent()
}
