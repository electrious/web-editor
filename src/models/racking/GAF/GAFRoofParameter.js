const {
    GAFParameter
} = require('webpb/electrious/models/racking/roof_parameter_pb')

exports.mkGAFParameterPB = _ => {
    return new GAFParameter()
}