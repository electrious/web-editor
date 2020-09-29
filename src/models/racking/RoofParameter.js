const {
    RoofParameter
} = require('webpb/electrious/models/racking/roof_parameter_pb')

exports.mkRoofParameterPB = _ => {
    return new RoofParameter()
}

exports.paramTypeNotSet = RoofParameter.ParamTypeCase.PARAM_TYPE_NOT_SET
exports.paramTypeXR = RoofParameter.ParamTypeCase.XR_PARAM
exports.paramTypeFX = RoofParameter.ParamTypeCase.FX_PARAM
exports.paramTypeXRFlat = RoofParameter.ParamTypeCase.XR_FLAT_PARAM
exports.paramTypeBX = RoofParameter.ParamTypeCase.BX_PARAM
exports.paramTypeGAF = RoofParameter.ParamTypeCase.GAF_PARAM


exports.toTagged = obj => {
    let k = Object.keys(obj)[0]
    return { tag: k, contents: obj[k] }
}