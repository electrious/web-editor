const { Clamp } = require('webpb/electrious/models/racking/clamp_pb')

exports.mkClampPB = _ => {
    return new Clamp()
}

exports.clampKindInvalid = Clamp.Kind.KIND_INVALID
exports.clampKindMiddle = Clamp.Kind.KIND_MIDDLE
exports.clampKindEnd = Clamp.Kind.KIND_END
