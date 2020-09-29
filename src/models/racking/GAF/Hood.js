const { Hood, Kind } = require('webpb/electrious/models/racking/hood_pb')

exports.mkHoodPB = _ => {
    return new Hood()
}

exports.hoodKindInvalid = Kind.KIND_INVALID
exports.hoodKindTop = Kind.KIND_TOP
exports.hoodKindBottom = Kind.KIND_BOTTOM
