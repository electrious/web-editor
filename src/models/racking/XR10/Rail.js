const { Rail } = require('webpb/electrious/models/racking/rail_pb')

exports.mkRailPB = _ => {
    return new Rail()
}

exports.getPanels = r => {
    return r.getPanelsList()
}

exports.setPanels = a => r => _ => {
    r.setPanelsList(a)
}