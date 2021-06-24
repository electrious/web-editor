const three = require('three')

exports.invert = m => {
    var n = new three.Matrix4()
    return n.copy(m).invert();
}
