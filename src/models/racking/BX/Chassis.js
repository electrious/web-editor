const { Chassis } = require('webpb/electrious/models/racking/chassis_pb')

exports.mkChassisPB = _ => {
    return Chassis()
}

exports.chassisKindInvalid = Chassis.Kind.KIND_INVALID
exports.chassisKindTilt5 = Chassis.Kind.KIND_TILT_5
exports.chassisKindTilt10 = Chassis.Kind.KIND_TILT_10