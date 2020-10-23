exports.mkTriangle = i => j => k => {
    const x1 = i.x
    const y1 = i.y
    const x2 = j.x
    const y2 = j.y
    const x3 = k.x
    const y3 = k.y
    let xc = 0
    let yc = 0
    
    const fabsy1y2 = Math.abs(y1 - y2)
    const fabsy2y3 = Math.abs(y2 - y3)
    
    if (fabsy1y2 < Number.EPSILON) {
        const m2 = -((x3 - x2) / (y3 - y2))
        const mx2 = (x2 + x3) / 2
        const my2 = (y2 + y3) / 2
        xc = (x2 + x1) / 2
        yc = m2 * (xc - mx2) + my2
    } else if (fabsy2y3 < Number.EPSILON) {
        const m1 = -((x2 - x1) / (y2 - y1))
        const mx1 = (x1 + x2) / 2
        const my1 = (y1 + y2) / 2
        xc = (x3 + x2) / 2
        yc = m1 * (xc - mx1) + my1
    } else {
        const m1 = -((x2 - x1) / (y2 - y1))
        const m2 = -((x3 - x2) / (y3 - y2))
        const mx1 = (x1 + x2) / 2
        const mx2 = (x2 + x3) / 2
        const my1 = (y1 + y2) / 2
        const my2 = (y2 + y3) / 2
        xc = (m1 * mx1 - m2 * mx2 + my2 - my1) / (m1 - m2)
        
        if (fabsy1y2 > fabsy2y3) {
            yc = m1 * (xc - mx1) + my1
        } else {
            yc = m2 * (xc - mx2) + my2
        }
    }
    
    const dx = x2 - xc
    const dy = y2 - yc
    const rsqr = dx * dx + dy * dy
    
    return { vertex1: i, vertex2: j, vertex3: k, x: xc, y: yc, rsqr: rsqr }
}

exports.triVertex1 = t => {
    return t.vertex1
}

exports.triVertex2 = t => {
    return t.vertex2
}

exports.triVertex3 = t => {
    return t.vertex3
}