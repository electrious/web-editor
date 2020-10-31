const three = require('three')

exports.mkColor = i => _ => {
    return new three.Color(i)
}

exports.mkColorString = s => _ => {
    return new three.Color(s)
}

exports.mkColorRGB = r => g => b => _ => {
    return new three.Color(r, g, b)
}