const { UUID } = require('webpb/electrious/uuid_pb')

exports.mkPBUUID = _ => {
    return new UUID()
}
