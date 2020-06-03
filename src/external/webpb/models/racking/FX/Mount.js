const { Mount, Spacing } = require('webpb/electrious/models/racking/mount_pb')

exports.mkMountPB = _ => {
    return new Mount()
}

exports.spacing_Invalid = Spacing.SPACING_INVALID
exports.spacing_24 = Spacing.SPACING_24
exports.spacing_32 = Spacing.SPACING_32
exports.spacing_48 = Spacing.SPACING_48
exports.spacing_64 = Spacing.SPACING_64
exports.spacing_72 = Spacing.SPACING_72
