const earcut = require('earcut');

exports.earcut = vs => {
    if (vs.length > 3) {
        return new Uint16Array(earcut(vs, null, 3));
    } else {
        return new Uint16Array([]);
    }
};
