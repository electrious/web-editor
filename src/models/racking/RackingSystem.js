const {
    RackingSystem
} = require('webpb/electrious/models/racking_pb')

exports.mkRackingSystemPB = _ => {
    return new RackingSystem()
}

exports.getRoofRackings = r => {
    return r.getRoofRackings()
}

exports.setRoofRackings = rr => r => _ => {
    r.setRoofRackings(rr)
}