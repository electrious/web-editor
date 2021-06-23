const earcut = require('earcut');

exports.earcut = vs => {
    if (vs.length > 9) {
        // there's more than 3 vertices, use earcut.
        return new Uint16Array(earcut(vs, null, 3));
    } else {
        // maybe just 3 vertices, just a single triangle
        return new Uint16Array([0, 1, 2]);
    }
};
