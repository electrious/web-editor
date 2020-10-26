const { Chassis } = require('webpb/electrious/models/racking/chassis_pb')

exports.mkChassisPB = _ => {
    return Chassis()
}

exports.chassisKindInvalid = Chassis.Kind.KIND_INVALID
exports.chassisKindTilt5 = Chassis.Kind.KIND_TILT_5
exports.chassisKindTilt10 = Chassis.Kind.KIND_TILT_10

exports.getTilt = c => {
    return c.getTilt()
}

exports.setTilt = t => c => _ => {
    c.setTilt(t)
}

exports.getKind = c => {
    return c.getKind()
}

exports.setKind = k => c => _ => {
    c.setKind(k)
}