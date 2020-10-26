const RBush = require('rbush')

exports.mkRBush = _ => {
    return new RBush()
}

exports.load = items => tree => _ => {
    tree.load(items)
}

exports.insert = item => tree => _ => {
    tree.insert(item)
}

exports.doSearch = box => tree => {
    return tree.search(box)
}
