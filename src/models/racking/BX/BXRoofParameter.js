const {
    BXParameter
} = require('webpb/electrious/models/racking/roof_parameter_pb')

exports.mkBXParameterPB = _ => {
    return new BXParameter()
}
