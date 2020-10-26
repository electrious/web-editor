const { Mount } = require('webpb/electrious/models/racking/mount_pb')

exports.mkMountPB = _ => {
    return new Mount()
}

exports.getFlash = m => {
    return m.getFlash()
}

exports.setFlash = f => m => _ => {
    m.setFlash(f)
}

exports.getClampX = m => {
    return m.getClampX()
}

exports.setClampX = x => m => _ => {
    m.setClampX(x)
}