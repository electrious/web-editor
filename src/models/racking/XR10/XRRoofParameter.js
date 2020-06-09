const {
    XRParameter
} = require('webpb/electrious/models/racking/roof_parameter_pb')

exports.mkXRParameterPB = _ => {
    return new XRParameter()
}
