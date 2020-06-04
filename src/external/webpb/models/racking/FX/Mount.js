const { Mount } = require('webpb/electrious/models/racking/mount_pb')

exports.mkMountPB = _ => {
    return new Mount()
}

exports.spacing_Invalid = Mount.Spacing.SPACING_INVALID
exports.spacing_24 = Mount.Spacing.SPACING_24
exports.spacing_32 = Mount.Spacing.SPACING_32
exports.spacing_48 = Mount.Spacing.SPACING_48
exports.spacing_64 = Mount.Spacing.SPACING_64
exports.spacing_72 = Mount.Spacing.SPACING_72
