const { QBaseMount } = require('webpb/electrious/models/racking/qbasemount_pb')

exports.mkQBaseMountPB = _ => {
    return new QBaseMount()
}