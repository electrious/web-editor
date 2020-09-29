const { Flash } = require('webpb/electrious/models/racking/flash_pb')

exports.mkFlashPB = _ => {
    return new Flash()
}
