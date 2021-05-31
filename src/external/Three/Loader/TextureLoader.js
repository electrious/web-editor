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

exports.textureDataURI = t => {
    let img = t.image;
    
     // Create an empty canvas element
    var canvas = document.createElement("canvas");
    canvas.width = img.width;
    canvas.height = img.height;

    // Copy the image contents to the canvas
    var ctx = canvas.getContext("2d");
    ctx.drawImage(img, 0, 0);

    // Get the data-URL formatted image
    // Firefox supports PNG and JPEG. You could check img.src to
    // guess the original format, but be aware the using "image/jpg"
    // will re-encode the image.
    return canvas.toDataURL("image/png");
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
