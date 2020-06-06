const { RoofRackingResult, RackingSystem } = require('webpb/electrious/models/racking_pb')

exports.mkRoofRackingResultPB = _ => {
    return new RoofRackingResult()
}

exports.mkRackingSystemPB = _ => {
    return new RackingSystem()
}