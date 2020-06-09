const { Rafter } = require('webpb/electrious/models/racking/rafter_pb')

exports.mkRafterPB = _ => {
    return new Rafter()
}
