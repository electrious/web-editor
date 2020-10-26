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

exports.getParamTypeCase = r => {
    return r.getParamTypeCase()
}

exports.getXRParameter = r => {
    return r.getXrParam()
}

exports.setXRParameter = x => r => _ => {
    r.setXrParam(x)
}

exports.getFXParameter = r => {
    return r.getFxParam()
}

exports.setFXParameter = x => r => _ => {
    r.setFxParam(x)
}

exports.getXRFlatParameter = r => {
    return r.getXrFlatParam()
}

exports.setXRFlatParameter = x => r => _ => {
    r.setXrFlatParam(x)
}

exports.getBXParameter = r => {
    return r.getBxParam()
}

exports.setBXParameter = x => r => _ => {
    r.setBxParam(x)
}

exports.getGAFParameter = r => {
    return r.getGafParam()
}

exports.setGAFParameter = x => r => _ => {
    r.setGafParam(x)
}

exports.toTagged = obj => {
    let k = Object.keys(obj)[0]
    return { tag: k, contents: obj[k] }
}