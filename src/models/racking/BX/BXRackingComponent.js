const {
    BallastComponent
} = require('webpb/electrious/models/racking/component_pb')

exports.mkBallastComponentPB = _ => {
    return new BallastComponent()
}

exports.getChassis = b => {
    return b.getChassisList()
}

exports.setChassis = cs => b => _ => {
    b.setChassisList(cs)
}

exports.getBlocks = b => {
    return b.getBlocksList()
}

exports.setBlocks = bs => b => _ => {
    b.setBlocksList(bs)
}