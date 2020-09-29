const { Skirt } = require('webpb/electrious/models/racking/skirt_pb')

exports.mkSkirtPB = _ => {
    return new Skirt()
}