const {
    RackingSystem
} = require('webpb/electrious/models/racking_pb')

exports.mkRackingSystemPB = _ => {
    return new RackingSystem()
}