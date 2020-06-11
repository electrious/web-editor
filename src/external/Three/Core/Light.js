const three = require('three')

exports.mkAmbientLight = c => _ => {
    return new three.AmbientLight(c)
}

exports.mkDirectionalLight = color => intensity => _ => {
    return new three.DirectionalLight(color, intensity)
}
