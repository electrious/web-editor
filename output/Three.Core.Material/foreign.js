let three = require('three')

exports.mkMeshBasicMaterial = c => {
    return _ => {
        return new three.MeshBasicMaterial({color: c})
    }
}