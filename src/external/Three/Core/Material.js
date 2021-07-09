let three = require('three')
let lines = require('three/examples/jsm/lines/LineMaterial')

exports.mkMeshBasicMaterial = c => _ => {
    return new three.MeshBasicMaterial({ color: c })
}

exports.mkMeshBasicMaterialWithColor = c => _ => {
    return new three.MeshBasicMaterial({ color: c })
}

exports.mkMeshBasicMaterialWithTexture = t => _ => {
    return new three.MeshBasicMaterial({ map: t })
}

exports.mkMeshPhongMaterial = c => _ => {
    return new three.MeshPhongMaterial({ color: c })
}

exports.mkLineBasicMaterial = c => w => _ => {
    return new lines.LineMaterial({ color: c, linewidth: 0.001 * w })
}

exports.mkLineDashedMaterial = c => w => s => ds => gs => _ => {
    return new lines.LineMaterial({ color: c, linewidth: 0.001 * w, dashed: true, dashSize: ds, gapSize: gs })
}

exports.jssetTransparent = t => mat => _ => {
    mat.transparent = t
}

exports.jssetOpacity = o => mat => _ => {
    mat.opacity = o
}

exports.jssetDepthWrite = d => mat => _ => {
    mat.depthWrite = d
}

exports.frontSide  = three.FrontSide
exports.backSide   = three.BackSide
exports.doubleSide = three.DoubleSide

exports.jssetSide = s => mat => _ => {
    mat.side = s
}

exports.jsgetMaterial = mat => creator => {
    return creator.materials[mat]
}

exports.preload = creator => _ => {
    creator.preload()
}
