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

exports.showVec2 = v => {
    return '(' + v.x + ', ' + v.y + ')'
}

exports.showVec3 = v => {
    return '(' + v.x + ', ' + v.y + ', ' + v.z + ')'
}

exports.jsClone = v => {
    return v.clone()
}

exports.vEq = v1 => v2 => {
    return v1.equals(v2)
}

exports.jsLength = v => {
    return v.length()
}

exports.jsDist = v1 => v2 => {
    return v1.distanceTo(v2)
}

exports.jsDot = v1 => v2 => {
    return v1.dot(v2)
}

exports.jsCross = v1 => v2 => {
    const r = v1.clone()
    r.cross(v2)

    return r
}

exports.jsAdd = v1 => v2 => {
    const r = v1.clone()
    r.add(v2)
    return r
}

exports.jsAddScaled = v1 => v2 => s => {
    const r = v1.clone()
    r.addScaledVector(v2, s)
    return r
}

exports.jsSub = v1 => v2 => {
    const r = v1.clone()
    r.sub(v2)
    return r
}

exports.jsMultiplyScalar = v => s => {
    const r = v.clone()
    r.multiplyScalar(s)
    return r
}

exports.jsNormal = v => {
    const r = v.clone()
    r.normalize()
    return r
}

exports.applyMatrix = m => v => {
    const nv = v.clone()
    return nv.applyMatrix4(m)
}
