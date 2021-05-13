const three = require('three')

exports.mkTextureLoader = _ => {
    return new three.TextureLoader()
}

exports.loadTexture = url => loader => _ => {
    return loader.load(url)
}

exports.loadTextureAsync = url => loader => cb => _ => {
    loader.load(url, t => { cb(t)() })
}

exports.dispose = t => _ => {
    t.dispose()
}

exports.textureWidth = t => {
    return t.image.width
}

exports.textureHeight = t => {
    return t.image.height
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
