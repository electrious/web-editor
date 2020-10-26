const {
    BXParameter
} = require('webpb/electrious/models/racking/roof_parameter_pb')

exports.mkBXParameterPB = _ => {
    return new BXParameter()
}

exports.getChassisKind = b => {
    return b.getChassisKind()
}

exports.setChassisKind = k => b => _ => {
    b.setChassisKind(k)
}