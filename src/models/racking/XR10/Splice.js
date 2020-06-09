const { Splice } = require('webpb/electrious/models/racking/splice_pb')

exports.mkSplicePB = _ => {
    return new Splice()
}