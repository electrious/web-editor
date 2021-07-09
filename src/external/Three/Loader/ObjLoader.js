const OBJLoader = require('three/examples/jsm/loaders/OBJLoader')
const MTLLoader = require('three/examples/jsm/loaders/MTLLoader')

exports.makeMTLLoader = _ => {
    return new MTLLoader.MTLLoader()
}

exports.makeOBJLoader = _ => {
    return new OBJLoader.OBJLoader()
}

exports.setPath = path => loader => _ => {
    loader.setPath(path)
}

exports.loadMTL = loader => name => cb => _ => {
    loader.load(name, function(mat) { cb(mat)() })
}

exports.jsloadOBJ = loader => name => cb => _ => {
    loader.load(name, function(obj) { cb(obj)() })
}

exports.parseOBJ = c => loader => {
    return loader.parse(c)
}

exports.setMaterials = matCreator => loader => _ => {
    loader.setMaterials(matCreator);
};
