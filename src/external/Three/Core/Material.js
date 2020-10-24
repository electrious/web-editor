
let three = require('three')

exports.mkMeshBasicMaterial = c => _ => {
    return new three.MeshBasicMaterial({ color: c })
}

exports.mkMeshBasicMaterialWithTexture = t => _ => {
    return new three.MeshBasicMaterial({ map: t })
}

exports.setTransparent = _ => t => mat => _ => {
    mat.transparent = t
}

exports.setOpacity = _ => o => mat => _ => {
    mat.opacity = o
}

exports.getMaterial = _ => mat => creator => {
    return creator.materials[mat]
}

exports.preload = creator => _ => {
    creator.preload()
}