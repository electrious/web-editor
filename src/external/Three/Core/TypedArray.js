exports.vector3Array = vs => {
    let arr = new Float32Array(vs.length * 3);

    for (var i = 0; i < vs.length; i++) {
        let v = vs[i];

        let j = 3 * i;
        
        arr[j] = v.x;
        arr[j + 1] = v.y;
        arr[j + 2] = v.z;
    }

    return arr;
}

exports.vector2Array = vs => {
    let arr = new Float32Array(vs.length * 2);

    for (var i = 0; i < vs.length; i++) {
        let v = vs[i];
        let j = 2 * i;

        arr[j] = v.x;
        arr[j + 1] = v.y;
    }

    return arr;
}
