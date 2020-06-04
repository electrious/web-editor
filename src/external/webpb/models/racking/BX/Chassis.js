const { Chassis, Kind } = require('webpb/electrious/models/racking/chassis_pb')

exports.mkChassisPB = _ => {
    return Chassis()
}

exports.chassisKindInvalid = Kind.KIND_INVALID
exports.chassisKindNorth = Kind.KIND_NORTH
exports.chassisKindSouth = Kind.KIND_SOUTH