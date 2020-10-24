const three = require('three')

exports.mkWebGLRenderer = _ => {
    return new three.WebGLRenderer({
        antialias: true,
        preserveDrawingBuffer: true
    })
}

exports.toDataUrl = img => canvas => _ => {
    return canvas.toDataURL(img)
}

exports.setSize = w => h => r => _ => {
    r.setSize(w, h)
}

exports.domElement = r => {
    return r.domElement
}

exports.render = _ => scene => camera => r => _ => {
    r.render(scene, camera)
}