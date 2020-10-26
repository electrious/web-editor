const {
    GAFComponent
} = require('webpb/electrious/models/racking/component_pb')

exports.mkGAFComponentPB = _ => {
    return new GAFComponent()
}

exports.getHoods = r => {
    return r.getHoodsList()
}

exports.setHoods = hs => r => _ => {
    r.setHoodsList(hs)
}