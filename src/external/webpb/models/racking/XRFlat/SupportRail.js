const { SupportRail } = require('webpb/electrious/models/racking/supportrail_pb')

exports.mkSupportRailPB = _ => {
    return new SupportRail()
}