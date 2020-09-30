const three = require('three')

exports.mkEuler = x => y => z => {
    // use 'ZYX' order so all Euler angle rotations are same with SceneKit
    return new three.Euler(x, y, z, 'ZYX')
}

exports.clone = e => {
    return e.clone()
}

exports.equal = e1 => e2 => {
    return e1.equal(e2)
}
