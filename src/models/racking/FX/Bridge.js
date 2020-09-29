const { Bridge } = require('webpb/electrious/models/racking/bridge_pb')

exports.mkBridgePB = _ => {
    return new Bridge()
}