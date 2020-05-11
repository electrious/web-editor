exports.toTagged = obj => {
    let k = Object.keys(obj)[0]
    return { tag: k, contents: obj[k] }
}