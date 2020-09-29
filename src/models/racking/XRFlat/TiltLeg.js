const { TiltLeg, Kind } = require('webpb/electrious/models/racking/tiltleg_pb')

exports.mkTiltLegPB = _ => {
    return new TiltLeg()
}

exports.tiltKindInvalid = Kind.KIND_INVALID
exports.tiltKindNorth = Kind.KIND_NORTH
exports.tiltKindSouth = Kind.KIND_SOUTH