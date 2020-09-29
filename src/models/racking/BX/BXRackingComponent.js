const {
    BallastComponent
} = require('webpb/electrious/models/racking/component_pb')

exports.mkBallastComponentPB = _ => {
    return new BallastComponent()
}