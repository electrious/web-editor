const three = require('three')

exports.mkAmbientLight = c => {
    return _ => {
        return new three.AmbientLight(c)
    }
}

exports.mkDirectionalLight = color => {
    return intensity => {
        return _ => {
            return new three.DirectionalLight(color, intensity)
        }
    }
}