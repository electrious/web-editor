const e = require('earcut')

exports.earcut = vs => {
    return e.earcut(vs, null, 3)
}
