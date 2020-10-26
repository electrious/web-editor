exports.getUUID = _ => r => {
    return r.getId()
}

exports.setUUID = _ => u => r => _ => {
    r.setId(u)
}

exports.getArrayNumber = _ => r => {
    return r.getArrayNumber()
}

exports.setArrayNumber = _ => n => r => _ => {
    r.setArrayNumber(n)
}

exports.getX = _ => r => {
    return r.getX()
}

exports.getY = _ => r => {
    return r.getY()
}

exports.getZ = _ => r => {
    return r.getZ()
}

exports.setX = _ => x => r => _ => {
    r.setX(x)
}

exports.setY = _ => y => r => _ => {
    r.setY(y)
}

exports.setZ = _ => z => r => _ => {
    r.setZ(z)
}

exports.getLength = _ => r => {
    return r.getLength()
}

exports.setLength = _ => l => r => _ => {
    r.setLength(l)
}

exports.getPos = _ => r => {
    return r.getPos()
}

exports.setPos = _ => p => r => _ => {
    r.setPos(p)
}
