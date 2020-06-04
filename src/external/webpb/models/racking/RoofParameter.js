const { RoofParameter,
        XRParameter,
        FXParameter,
        XRFlatParameter,
        BXParameter,
        GAFParameter
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

exports.mkXRParameterPB = _ => {
    return new XRParameter()
}

exports.mkFXParameterPB = _ => {
    return new FXParameter()
}

exports.mkXRFlatParameterPB = _ => {
    return new XRFlatParameter()
}

exports.mkBXParameterPB = _ => {
    return new BXParameter()
}

exports.mkGAFParameterPB = _ => {
    return new GAFParameter()
}