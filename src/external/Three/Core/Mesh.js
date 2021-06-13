const three = require('three')
const lines = require('three/examples/jsm/lines/Line2')

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

exports.material = mesh => {
    return mesh.material;
}

exports.setMaterial = _ => mat => mesh => _ => [
    mesh.material = mat
]

exports.mkLine = _ => geo => mat => _ => {
    return new lines.Line2(geo, mat)
}

exports.computeLineDistances = l => _ => {
    l.computeLineDistances()
}
