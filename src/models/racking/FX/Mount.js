const { Mount } = require('webpb/electrious/models/racking/mount_pb')

exports.mkMountPB = _ => {
    return new Mount()
}