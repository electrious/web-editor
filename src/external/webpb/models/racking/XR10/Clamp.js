const { Clamp, Kind } = require('webpb/electrious/models/racking/clamp_pb')

exports.mkClampPB = _ => {
    return new Clamp()
}

exports.kind_Invalid = Kind.KIND_INVALID
exports.kind_Middle = Kind.KIND_MIDDLE
exports.kind_End = Kind.KIND_END
