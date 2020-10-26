const { Map } = require('google-protobuf')

exports.mkMapPB = _ => {
    return new Map()
}

exports.toArray = m => {
    return m.toArray()
}

exports.set = k => v => m => _ => {
    m.set(k, v)
}

exports.key = a => {
    return a[0]
}

exports.value = a => {
    return a[1]
}