const three = require('three')

exports.mkVec2 = x => {
    return y => {
        return new three.Vector2(x, y)
    }
}

exports.mkVec3 = x => {
    return y => {
        return z => {
            return new three.Vector3(x, y, z)
        }
    }
}

exports.vClone = v => {
    return v.clone()
}

exports.vEq = v1 => {
    return v2 => {
        return v1.equals(v2)
    }
}

exports.vCross = v1 => {
    return v2 => {
        const r = v1.clone()
        r.cross(v2)

        return r
    }
}

exports.vAdd = v1 => {
    return v2 => {
        const r = v1.clone()
        r.add(v2)
        return r
    }
}

exports.vAddScaled = v1 => {
    return v2 => {
        return s => {
            const r = v1.clone()
            r.addScaledVector(v2, s)
            return r
        }
    }
}

exports.vSub = v1 => {
    return v2 => {
        const r = v1.clone()
        r.sub(v2)
        return r
    }
}

exports.vMultiplyScalar = v => {
    return s => {
        const r = v.clone()
        r.multiplyScalar(s)
        return r
    }
}

exports.vNormal = v => {
    const r = v.clone()
    r.normalize()
    return r
}

exports.applyMatrix = m => {
    return v => {
        const nv = v.clone()
        return nv.applyMatrix4(m)
    }
}
