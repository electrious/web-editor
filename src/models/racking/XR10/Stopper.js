const { Stopper } = require('webpb/electrious/models/racking/stopper_pb')

exports.mkStopperPB = _ => {
    return new Stopper()
}