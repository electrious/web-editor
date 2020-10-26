const { RailFreeComponent } = require('webpb/electrious/models/racking/component_pb')

exports.mkRailFreeComponentPB = _ => {
    return new RailFreeComponent()
}

exports.getMounts = r => {
    return r.getMountsList()
}

exports.setMounts = ms => r => _ => {
    r.setMountsList(ms)
}

exports.getBridges = r => {
    return r.getBridgesList()
}

exports.setBridges = bs => r => _ => {
    r.setBridgesList(bs)
}

exports.getSkirts = r => {
    return r.getSkirtsList()
}

exports.setSkirts = ss => r => _ => {
    r.setSkirtsList(ss)
}

exports.getLeftCaps = r => {
    return r.getLeftCapsList()
}

exports.setLeftCaps = cs => r => _ => {
    r.setLeftCapsList(cs)
}

exports.getRightCaps = r => {
    return r.getRightCapsList()
}

exports.setRightCaps = cs => r => _ => {
    r.setRightCapsList(cs)
}
