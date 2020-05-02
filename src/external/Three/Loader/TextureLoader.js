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