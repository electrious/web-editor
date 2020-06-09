const { EndCap } = require('webpb/electrious/models/racking/endcap_pb')

exports.mkEndCapPB = _ => {
    return new EndCap()
}