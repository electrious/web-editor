const { DoRackRequest, DoRackResponse } = require('webpb/electrious/api/v1/racking_api_pb')
const { RackingAPIClient } = require('webpb/electrious/api/v1/racking_api_grpc_web_pb')

exports.mkRackingAPIClient = host => _ => {
    return new RackingAPIClient(host)
}

exports.doRack = req => f => client => _ => {
    client.doRack(req, undefined, (err, resp) => {
        f(err)(resp)()
    })
}

exports.mkDoRackRequestPB = _ => {
    return new DoRackRequest()
}

exports.mkDoRackResponsePB = _ => {
    return new DoRackResponse()
}

exports.getRequest = r => {
    return r.getRequest()
}

exports.setRequest = rq => r => _ => {
    r.setRequest(rq)
}

exports.getRacking = r => {
    return r.getRacking()
}

exports.setRacking = rs => r => _ => {
    r.setRacking(rs)
}