const three = require('three')

exports.mkMesh = geo => {
    return mat => {
        return _ => {
            return new three.Mesh(geo, mat)
        }
    }
}

exports.isMesh = m => {
    return m instanceof three.Mesh
}