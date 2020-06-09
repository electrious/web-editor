const { Rail } = require('webpb/electrious/models/racking/rail_pb')

exports.mkRailPB = _ => {
    return new Rail()
}