exports.getArrayNumber = _ => a => {
    return a.getArrayNumber()
}

exports.setArrayNumber = _ => n => a => _ => {
    a.setArrayNumber(n)
}

exports.getRails = _ => a => {
    return a.getRailsList()
}

exports.setRails = _ => arr => a => _ => {
    a.setRailsList(arr)
}

exports.getFullRailsNum = _ => r => {
    return r.getFullRailsNum()
}

exports.setFullRailsNum = _ => n => a => _ => {
    a.setFullRailsNum(n)
}

exports.getFlashes = _ => a => {
    return a.getFlashesList()
}

exports.setFlashes = _ => arr => a => _ => {
    a.setFlashesList(arr)
}

exports.getSplices = _ => a => {
    return a.getSplicesList()
}

exports.setSplices = _ => arr => a => _ => {
    a.setSplicesList(arr)
}

exports.getClamps = _ => a => {
    return a.getClampsList()
}

exports.setClamps = _ => arr => a => _ => {
    a.setClampsList(arr)
}

exports.getStoppers = _ => a => {
    return a.getStoppersList()
}

exports.setStoppers = _ => arr => a => _ => {
    a.setStoppersList(arr)
}

exports.getMountSpacing = _ => a => {
    return a.getMountSpace()
}

exports.setMountSpacing = _ => s => a => _ => {
    a.setMountSpace(s)
}

exports.getRafterSpacing = _ => a => {
    return a.getRafterSpace()
}

exports.setRafterSpacing = _ => s => a => _ => {
    a.setRafterSpace(s)
}
