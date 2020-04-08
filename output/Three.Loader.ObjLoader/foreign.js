const OBJLoader2 = require('three/examples/jsm/loaders/OBJLoader2')
const MTLLoader = require('three/examples/jsm/loaders/MTLLoader')

exports.makeMTLLoader = _ => {
    return new MTLLoader.MTLLoader()
}

exports.makeOBJLoader2 = _ => {
    return new OBJLoader2.OBJLoader2()
}