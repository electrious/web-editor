const { mkTriangle } = require("./Triangle")

function eqNum(n1, n2) {
    return Math.abs(n1 - n2) < 0.000001
}

function eqVec(v1, v2) {
    return eqNum(v1.x, v2.x) && eqNum(v1.y, v2.y)
}

function dedup(e) {
    var a, b, m, n
    
    var j = e.length
    while (j > 1) {
        j -= 1
        b = e[j]
        j -= 1
        a = e[j]
        
        var i = j
        while (i > 1) {
            i -= 1
            n = e[i]
            i -= 1
            m = e[i]
            
            if ((eqVec(a, m) && eqVec(b, n)) || (eqVec(a, n) && eqVec(b, m))) {
                e.splice(j, 2)
                e.splice(i, 2)
                
                j -= 2 // the two vertices indexed by i and i+1 are deleted. skip them
                
                break
            }
        }
    }
    
    return e
}

exports.triangulate = old => vertices => {
    var open = old
    var completed = []
    var edges = []
    
    // vertices sorted by the x position
    const vertSorted = vertices.sort((v1, v2) => { return v1.x - v2.x })
    
    // Incrementally add each vertex to the mesh.
    for (const v of vertSorted) {
        edges = []
        
        // For each open triangle, check to see if the current point is
        // inside its circumcircle. If it is, remove the triangle and add
        // it's edges to an edge list.
        
        for (var j = open.length - 1; j >= 0; j--) {
            const circle = open[j]
            
            // If this point is to the right of this triangle's circumcircle,
            // then this triangle should never get checked again. Remove it
            // from the open list, add it to the closed list, and skip.
            const dx = v.x - circle.x
            const dy = v.y - circle.y

            if (dx > 0 && dx * dx > circle.rsqr) {
                open.splice(j, 1)
                completed.push(circle)
                continue
            }
            
            // If we're outside the circumcircle, skip this triangle.            
            if (dx * dx + dy * dy > circle.rsqr) {
                continue
            }
            
            // Remove the triangle and add its edges to the edge list.
            edges = edges.concat([
                circle.vertex1, circle.vertex2,
                circle.vertex2, circle.vertex3,
                circle.vertex3, circle.vertex1
            ])
            
            open.splice(j, 1)
        }
        
        // Remove any doubled edges.
        edges = dedup(edges)
        
        // Add a new triangle for each edge.
        var j = edges.length
        while (j > 1) {
            j -= 1
            let b = edges[j]
            j -= 1
            let a = edges[j]
            open.push(mkTriangle(a)(b)(v))
        }
    }
    
    /* Copy any remaining open triangles to the closed list, and then
    * remove any triangles that share a vertex with the supertriangle,
    * building a list of triplets that represent triangles. */
    return completed.concat(open)
}
