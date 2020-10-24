const three = require('three')

exports.mkScene = _ => {
    return new three.Scene()
}

exports.disposeScene = s => _ => {
    s.dispose()
}