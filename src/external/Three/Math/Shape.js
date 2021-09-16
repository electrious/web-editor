const three = require('three');

exports.mkShape = _ => {
    return new three.Shape();
}

exports.mkShapeWith = ps => _ => {
    return new three.Shape(ps)
}
