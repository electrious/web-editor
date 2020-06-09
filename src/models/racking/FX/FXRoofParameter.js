const {
    FXParameter
} = require('webpb/electrious/models/racking/roof_parameter_pb')

exports.mkFXParameterPB = _ => {
    return new FXParameter()
}
