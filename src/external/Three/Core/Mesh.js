const three = require('three')

exports.mkMesh = _ => _ => geo => mat => _ => {
    return new three.Mesh(geo, mat)
}

exports.isMesh = m => {
    return m instanceof three.Mesh
}