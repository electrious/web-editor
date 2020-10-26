const OBJLoader2 = require('three/examples/jsm/loaders/OBJLoader2')
const MTLLoader = require('three/examples/jsm/loaders/MTLLoader')

exports.makeMTLLoader = _ => {
    return new MTLLoader.MTLLoader()
}

exports.makeOBJLoader2 = _ => {
    return new OBJLoader2.OBJLoader2()
}

exports.setPath = path => loader => _ => {
    loader.setPath(path)
}

exports.loadMTL = loader => name => cb => _ => {
    loader.load(name, function(mat) { cb(mat)() })
}

exports.loadOBJ = loader => name => cb => _ => {
    loader.load(name, function(obj) { cb(obj)() })
}

exports.parseOBJ = c => loader => {
    return loader.parse(c)
}
