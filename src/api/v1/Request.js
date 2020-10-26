const { RackRequest } = require('webpb/electrious/workers/racking/request_pb')

exports.mkRackRequestPB = _ => {
    return RackRequest()
}

exports.getParams = r => {
    return r.getParams()
}

exports.setParams = p => r => _ => {
    r.setParams(p)
}

exports.getPanels = r => {
    return r.getPanelsList()
}

exports.setPanels = ps => r => _ => {
    r.setPanelsList(ps)
}