const { UUID } = require('webpb/electrious/uuid_pb')

exports.mkPBUUID = _ => {
    return new UUID()
}

exports.getUUIDString = u => {
    return u.getUuid()
}
exports.setUUIDString = v => u => _ => {
    u.setUuid(v)
}