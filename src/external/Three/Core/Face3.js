exports.mkFace3 = a => b => c => _ => {
    return { a: a, b: b, c: c };
};

exports.indexA = f => {
    return f.a
}

exports.indexB = f => {
    return f.b
}

exports.indexC = f => {
    return f.c
}

exports.normal = f => {
    return f.normal
}
