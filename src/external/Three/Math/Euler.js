const three = require('three')

exports.mkEuler = x => y => z => {
    return new three.Euler(x, y, z)
}

exports.clone = e => {
    return e.clone()
}

exports.equal = e1 => e2 => {
    return e1.equal(e2)
}
