const {
    Component
} = require('webpb/electrious/models/racking/component_pb')

exports.mkComponentPB = _ => {
    return new Component()
}

exports.rdTypeNotSet = Component.RdTypeCase.RD_TYPE_NOT_SET
exports.rdTypeRail = Component.RdTypeCase.RAIL
exports.rdTypeRailFree = Component.RdTypeCase.RAIL_FREE
exports.rdTypeRailFlat = Component.RdTypeCase.RAIL_FLAT
exports.rdTypeBallast = Component.RdTypeCase.BALLAST
exports.rdTypeGAF = Component.RdTypeCase.GAF

const {
    RoofRackingResult
} = require('webpb/electrious/models/racking_pb')

exports.mkRoofRackingResultPB = _ => {
    return new RoofRackingResult()
}

exports.getRdTypeCase = c => {
    return c.getRdTypeCase()
}

exports.getRail = c => {
    return c.getRail()
}

exports.setRail = r => c => _ => {
    c.setRail(r)
}

exports.getRailFree = c => {
    return c.getRailFree()
}

exports.setRailFree = f => c => _ => {
    c.setRailFree(f)
}

exports.getRailFlat = c => {
    return c.getRailFlat()
}

exports.setRailFlat = f => c => _ => {
    c.setRailFlat(f)
}

exports.getBallast = c => {
    return c.getBallast()
}

exports.setBallast = b => c => _ => {
    c.setBallast(b)
}

exports.getGAF = c => {
    return c.getGaf()
}

exports.setGAF = g => c => _ => {
    c.setGaf(g)
}

exports.getKind = r => {
    return r.getKind()
}

exports.setKind = k => r => _ => {
    r.setKind(k)
}

exports.getRafters = r => {
    return r.getRaftersList()
}

exports.setRafters = rs => r => _ => {
    r.setRaftersList(rs)
}

exports.getParams = r => {
    return r.getParams()
}

exports.setParams = p => r => _ => {
    r.setParams(p)
}

exports.getComponents = r => {
    return r.getComponents()
}

exports.setComponents = c => r => _ => {
    r.setComponents(c)
}
