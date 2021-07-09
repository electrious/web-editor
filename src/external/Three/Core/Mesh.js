const three = require('three')
const lines = require('three/examples/jsm/lines/Line2')

exports.jsmkMesh = geo => mat => _ => {
    return new three.Mesh(geo, mat)
}

exports.isMesh = m => {
    return m instanceof three.Mesh
}

exports.jsgeometry = mesh => {
    return mesh.geometry
}

exports.jssetGeometry = geo => mesh => _ => {
    mesh.geometry = geo
}

exports.material = mesh => {
    return mesh.material;
}

exports.jssetMaterial = mat => mesh => _ => [
    mesh.material = mat
]

exports.jsmkLine = geo => mat => _ => {
    return new lines.Line2(geo, mat)
}

exports.computeLineDistances = l => _ => {
    l.computeLineDistances()
}
