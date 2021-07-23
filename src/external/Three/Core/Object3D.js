const three = require('three')

exports.mkObject3D = _ => {
    return new three.Object3D()
}

exports.setDefaultUp = v => _ => {
    three.Object3D.DefaultUp.copy(v)
}

exports.jsCastShadow = obj => {
    return obj.castShadow
}

exports.jsSetCastShadow = s => o => _ => {
    o.castShadow = s
}

exports.jsSetReceiveShadow = r => o => _ => {
    o.receiveShadow = r
}

exports.jsChildren = o => {
    return o.children
}

exports.jsHasParent = o => {
    return o.parent !== null && o.parent !== undefined
}

exports.jsParent = o => {
    return o.parent
}

exports.jsAdd = c => p => _ => {
    p.add(c)
}

exports.jsRemove = c => p => _ => {
    p.remove(c)
}

exports.jsSetName = name => o => _ => {
    o.name = name
}

exports.jsPosition = o => {
    return o.position
}

exports.jsSetPosition = v => o => _ => {
    o.position.copy(v)
}

exports.jsSetRotation = r => o => _ => {
    o.rotation.copy(r)
}

exports.jsSetScale = s => o => _ => {
    o.scale.copy(s)
}

exports.jsRotateX = r => o => _ => {
    o.rotateX(r)
}

exports.jsRotateY = r => o => _ => {
    o.rotateY(r)
}

exports.jsRotateZ = r => o => _ => {
    o.rotateZ(r)
}

exports.jsRotateOnWorldAxis = v => d => o => _ => {
    o.rotateOnWorldAxis(v, d)
}

exports.jsRotateWithEuler = e => o => _ => {
    o.setRotationFromEuler(e)
}

exports.jsTranslateX = x => o => _ => {
    o.translateX(x)
}

exports.jsTranslateY = y => o => _ => {
    o.translateY(y)
}

exports.jsTranslateZ = z => o => _ => {
    o.translateZ(z)
}

exports.jsSetRenderOrder = r => o => _ => {
    o.renderOrder = r
}

exports.jsSetVisible = v => o => _ => {
    o.visible = v
}

exports.jsMatrix = o => {
    return o.matrix
}

exports.jsUpdateMatrix = o => _ => {
    o.updateMatrix()
}

exports.jsUpdateMatrixWorld = o => _ => {
    o.updateMatrixWorld()
}

exports.jsLocalToWorld = v => o => _ => {
    return o.localToWorld(v.clone())
}

exports.jsWorldToLocal = v => o => _ => {
    return o.worldToLocal(v.clone())
}

exports.jsLookAt = v => o => _ => {
    o.lookAt(v)
}

exports.jsclone = o => _ => {
    return o.clone()
}

exports.jsUserData = o => {
    return o.userData
}

exports.jsSetUserData = d => o => _ => {
    o.userData = d
}

exports.jsEnableLayer = l => o => _ => {
    o.layers.enable(l);
}

exports.jsDisableLayer = l => o => _ => {
    o.layers.disable(l);
}

exports.jsSetExportable = o => _ => {
    o.exportable = true
}
