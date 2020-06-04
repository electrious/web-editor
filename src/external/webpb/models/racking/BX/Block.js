const { Block } = require('webpb/electrious/models/racking/block_pb')

exports.mkBlockPB = _ => {
    return new Block()
}