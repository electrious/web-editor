const { EndCap } = require('webpb/electrious/models/racking/endcap_pb')

exports.mkEndCapPB = _ => {
    return new EndCap()
}

exports.getSkirt = e => {
    return e.getSkirt()
}

exports.setSkirt = s => e => _ => {
    e.setSkirt(s)
}