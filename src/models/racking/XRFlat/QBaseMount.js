const { QBaseMount } = require('webpb/electrious/models/racking/qbasemount_pb')

exports.mkQBaseMountPB = _ => {
    return new QBaseMount()
}

exports.getHeight = q => {
    return q.getHeight()
}

exports.setHeight = h => q => _ => {
    q.setHeight(h)
}