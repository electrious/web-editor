const { LFoot } = require('webpb/electrious/models/racking/lfoot_pb')

exports.mkLFootPB = _ => {
    return new LFoot()
}

exports.getFlashId = l => {
    return l.getFlashId()
}

exports.setFlashId = u => l => _ => {
    l.setFlashId(u)
}