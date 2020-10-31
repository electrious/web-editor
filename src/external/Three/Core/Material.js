let three = require('three')

exports.mkMeshBasicMaterial = c => _ => {
    return new three.MeshBasicMaterial({ color: c })
}

exports.mkMeshBasicMaterialWithTexture = t => _ => {
    return new three.MeshBasicMaterial({ map: t })
}

exports.mkLineBasicMaterial = c => w => _ => {
    return new three.LineBasicMaterial({ color: c, linewidth: w })
}

exports.setTransparent = _ => t => mat => _ => {
    mat.transparent = t
}

exports.setOpacity = _ => o => mat => _ => {
    mat.opacity = o
}

exports.frontSide  = three.FrontSide
exports.backSide   = three.BackSide
exports.doubleSide = three.DoubleSide

exports.setSide = _ => s => mat => _ => {
    mat.side = s
}

exports.getMaterial = _ => mat => creator => {
    return creator.materials[mat]
}

exports.preload = creator => _ => {
    creator.preload()
}