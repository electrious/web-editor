const three = require('three')

exports.mkWebGLRenderer = _ => {
    return new three.WebGLRenderer()
}