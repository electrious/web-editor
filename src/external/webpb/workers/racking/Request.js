const { RackRequest } = require('webpb/electrious/workers/racking/request_pb')

exports.mkRackRequestPB = _ => {
    return RackRequest()
}
