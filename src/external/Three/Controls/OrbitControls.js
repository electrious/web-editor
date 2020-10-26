let o = require('three/examples/jsm/controls/OrbitControls')

exports.mkOrbitControls = _ => camera => domElem => _ => {
    return new o.OrbitControls(camera, domElem)
}

exports.update = o => _ => {
    o.update()
}

exports.dispose = o => _ => {
    o.dispose()
}

exports.setAutoRotate = r => o => _ => {
    o.autoRotate = r
}

exports.setAutoRotateSpeed = s => o => _ => {
    o.autoRotateSpeed = s
}

exports.isEnabled = o => {
    return o.enabled
}

exports.setEnabled = e => o => _ => {
    o.enabled = e
}

exports.enableDamping = e => o => _ => {
    o.enableDamping = e
}

exports.setDampingFactor = f => o => _ => {
    o.dampingFactor = f
}

exports.enablePan = e => o => _ => {
    o.enablePan = e
}

exports.setPanSpeed = s => o => _ => {
    o.panSpeed = s
}

exports.enableRotate = e => o => _ => {
    o.enableRotate = e
}

exports.setRotateSpeed = s => o => _ => {
    o.rotateSpeed = s
}

exports.enableZoom = e => o => _ => {
    o.enableZoom = e
}

exports.setZoomSpeed = s => o => _ => {
    o.zoomSpeed = s
}

exports.setMaxAzimuthAngle = a => o => _ => {
    o.maxAzimuthAngle = a
}

exports.setMinAzimuthAngle = a => o => _ => {
    o.minAzimuthAngle = a
}

exports.setMaxDistance = d => o => _ => {
    o.maxDistance = d
}

exports.setMinDistance = d => o => _ => {
    o.minDistance = d
}

exports.setMaxPolarAngle = a => o => _ => {
    o.maxPolarAngle = a
}

exports.setMinPolarAngle = a => o => _ => {
    o.minPolarAngle = a
}

exports.setTarget = t => o => _ => {
    o.target = t
}