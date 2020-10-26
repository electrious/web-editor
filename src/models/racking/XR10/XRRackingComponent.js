const { RailComponent } = require('webpb/electrious/models/racking/component_pb')

exports.mkRailComponentPB = _ => {
    return new RailComponent()
}

exports.getLFeet = r => {
    return r.getLfeetList()
}

exports.setLFeet = l => r => _ => {
    r.setLfeetList(l)
}