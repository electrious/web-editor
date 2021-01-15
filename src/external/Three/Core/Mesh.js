const three = require('three')

exports.mkMesh = _ => _ => geo => mat => _ => {
    return new three.Mesh(geo, mat)
}

exports.isMesh = m => {
    return m instanceof three.Mesh
}

exports.geometry = _ => mesh => {
    return mesh.geometry
}

exports.bufferGeometry = _ => mesh => {
    return mesh.geometry
}

exports.setBufferGeometry = _ => geo => mesh => _ => {
    mesh.geometry = geo
}

exports.setMaterial = _ => mat => mesh => _ => [
    mesh.material = mat
]

exports.mkLine = geo => mat => _ => {
    return new three.Line(geo, mat)
}
