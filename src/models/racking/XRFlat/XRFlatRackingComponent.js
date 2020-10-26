const {
    RailFlatComponent
} = require('webpb/electrious/models/racking/component_pb')

exports.mkRailFlatComponentPB = _ => {
    return new RailFlatComponent()
}

exports.getSupportRails = r => {
    return r.getSupportRailsList()
}

exports.setSupportRails = ss => r => _ => {
    r.setSupportRailsList(ss)
}

exports.getQBaseMounts = r => {
    return r.getQbaseMountsList()
}

exports.setQBaseMounts = qs => r => _ => {
    r.setQbaseMountsList(qs)
}

exports.getTiltLegs = r => {
    return r.getTiltLegsList()
}

exports.setTiltLegs = ts => r => _ => {
    r.setTiltLegsList(ts)
}
