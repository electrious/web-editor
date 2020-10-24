const three = require('three')

exports.mkVec2 = x => y => {
    return new three.Vector2(x, y)
}

exports.mkVec3 = x => y => z => {
    return new three.Vector3(x, y, z)
}

exports.vecX = _ => v => {
    return v.x
}

exports.vecY = _ => v => {
    return v.y
}

exports.vecZ = _ => v => {
    return v.z
}

exports.clone = _ => v => {
    return v.clone()
}

exports.vEq = v1 => v2 => {
    return v1.equals(v2)
}

exports.length = _ => v => {
    return v.length()
}

exports.dist = _ => v1 => v2 => {
    return v1.distanceTo(v2)
}

exports.dot = _ => v1 => v2 => {
    return v1.dot(v2)
}

exports.cross = _ => v1 => v2 => {
    const r = v1.clone()
    r.cross(v2)

    return r
}

exports.add = _ => v1 => v2 => {
    const r = v1.clone()
    r.add(v2)
    return r
}

exports.addScaled = _ => v1 => v2 => s => {
    const r = v1.clone()
    r.addScaledVector(v2, s)
    return r
}

exports.sub = _ => v1 => v2 => {
    const r = v1.clone()
    r.sub(v2)
    return r
}

exports.multiplyScalar = _ => v => s => {
    const r = v.clone()
    r.multiplyScalar(s)
    return r
}

exports.normal = _ => v => {
    const r = v.clone()
    r.normalize()
    return r
}

exports.applyMatrix = m => v => {
    const nv = v.clone()
    return nv.applyMatrix4(m)
}
