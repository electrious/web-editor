const { Flash } = require('webpb/electrious/models/racking/flash_pb')

exports.mkFlashPB = _ => {
    return new Flash()
}

exports.getRafterId = r => {
    return r.getRafterId()
}

exports.setRafterId = u => r => _ => {
    r.setRafterId(u)
}

exports.getClampTarget = r => {
    return r.getClampTarget()
}

exports.setClampTarget = t => r => _ => {
    r.setClampTarget(t)
}