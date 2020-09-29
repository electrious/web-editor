const {
    GAFComponent
} = require('webpb/electrious/models/racking/component_pb')

exports.mkGAFComponentPB = _ => {
    return new GAFComponent()
}