const { Chassis } = require('webpb/electrious/models/racking/chassis_pb')

exports.mkChassisPB = _ => {
    return Chassis()
}

exports.chassisKindInvalid = Chassis.Kind.KIND_INVALID
exports.chassisKindNorth = Chassis.Kind.KIND_NORTH
exports.chassisKindSouth = Chassis.Kind.KIND_SOUTH