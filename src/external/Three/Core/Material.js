let three = require('three')

exports.mkMeshBasicMaterial = c => _ => {
    return new three.MeshBasicMaterial({ color: c })
}

exports.mkMeshBasicMaterialWithTexture = t => _ => {
    return new three.MeshBasicMaterial({ map: t })
}
