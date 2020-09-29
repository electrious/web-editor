const {
    RailFlatComponent
} = require('webpb/electrious/models/racking/component_pb')

exports.mkRailFlatComponentPB = _ => {
    return new RailFlatComponent()
}
