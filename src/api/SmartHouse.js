exports.toFormData = req => {
    let d = new FormData();
    d.append("obj", req.obj);
    d.append("mtl", req.mtl);
    d.append("texture", req.texture);

    return d;
};
