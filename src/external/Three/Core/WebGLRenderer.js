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