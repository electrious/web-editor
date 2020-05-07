const three = require('three')

exports.mkObject3D = _ => {
    return new three.Object3D()
}

exports.setDefaultUp = v => _ => {
    three.Object3D.DefaultUp.copy(v)
}