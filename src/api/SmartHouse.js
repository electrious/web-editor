exports.toFormData = req => {
    let d = new FormData();
    d.append("obj", d.obj);
    d.append("mtl", d.mtl);
    d.append("texture", d.texture);

    return d;
};
