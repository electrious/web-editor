const { Rafter } = require('webpb/electrious/models/racking/rafter_pb')

exports.mkRafterPB = _ => {
    return new Rafter()
}

exports.rafterSpacingInvalid = Rafter.Spacing.SPACING_INVALID
exports.rafterSpacing16 = Rafter.Spacing.SPACING_16
exports.rafterSpacing24 = Rafter.Spacing.SPACING_24
