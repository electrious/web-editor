const three = require('three')

exports.mkFace3 = a => b => c => _ => {
    return new three.Face3(a, b, c)
}
