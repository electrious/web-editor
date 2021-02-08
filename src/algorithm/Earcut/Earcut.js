const e = require('earcut')
const three = require('three')

exports.earcut = vs => {
    const idxs = e.earcut(vs, null, 3)
    let faces = []

    let l = idxs.length
    for(var i = 0; i < l; i += 3) {
        faces.push(new three.Face3(idxs[i], idxs[i + 1], idxs[i + 2]))
    }

    return faces
}
