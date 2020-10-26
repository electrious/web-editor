const { Splice } = require('webpb/electrious/models/racking/splice_pb')

exports.mkSplicePB = _ => {
    return new Splice()
}

exports.getRails = s => {
    return s.getRailsList()
}

exports.setRails = a => s => _ => {
    s.setRailsList(a)
}