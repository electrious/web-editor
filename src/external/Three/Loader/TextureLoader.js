const three = require('three')

exports.mkTextureLoader = _ => {
    return new three.TextureLoader()
}

exports.loadTexture = url => loader => _ => {
    return loader.load(url)
}

exports.dispose = t => _ => {
    t.dispose()
}

// texture wrapping mode
exports.clampToEdgeWrapping = three.ClampToEdgeWrapping
exports.repeatWrapping = three.RepeatWrapping
exports.mirroredRepeatWrapping = three.MirroredRepeatWrapping

exports.setWrapS = w => t => _ => {
    t.wrapS = w
}

exports.setWrapT = w => t => _ => {
    t.wrapT = w
}

exports.setRepeat = w => h => t => _ => {
    t.repeat.set(w, h)
}
