const { TiltLeg, Kind } = require('webpb/electrious/models/racking/tiltleg_pb')

exports.mkTiltLegPB = _ => {
    return new TiltLeg()
}

exports.tiltKindInvalid = Kind.KIND_INVALID
exports.tiltKindNorth = Kind.KIND_NORTH
exports.tiltKindSouth = Kind.KIND_SOUTH

exports.getTilt = t => {
    return t.getTilt()
}

exports.setTilt = l => t => _ => {
    t.setTilt(l)
}

exports.getKind = t => {
    return t.getKind()
}

exports.setKind = k => t => _ => {
    t.setKind(k)
}