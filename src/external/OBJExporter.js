var THREE = require('three');

exports.exportObject = object => _ => {
	var output = '';
	var materials = {};

	var indexVertex = 0;
	var indexVertexUvs = 0;
        var indexNormals = 0;
    
        var inverseMatrixWorld = new THREE.Matrix4();
        inverseMatrixWorld.getInverse(object.matrixWorld.clone());
    
	output += 'mtllib scene.mtl\n';

	var parseMesh = function ( mesh ) {

	    var nbVertex = 0;
	    var nbVertexUvs = 0;
	    var nbNormals = 0;

	    var geometry = mesh.geometry;
	    var material = mesh.material;
            
            if (geometry instanceof THREE.BufferGeometry) {
                // name of the mesh object
		output += 'o ' + mesh.name + '\n';

                var vertices = geometry.getAttribute('position');
		var normals = geometry.getAttribute('normal');
		var uvs = geometry.getAttribute('uv');
		var indices = geometry.getIndex();

                var vertex = new THREE.Vector3();
		var normal = new THREE.Vector3();
		var uv = new THREE.Vector2();


                var i,
		    j,
		    l,
		    m,
                    v,
		    face = [];

                // vertices
                if (vertices !== undefined) {
                    for (i = 0, l = vertices.count; i < l; i++) {
                        vertex.x = vertices.getX(i);
                        vertex.y = vertices.getY(i);
                        vertex.z = vertices.getZ(i);

                        v = object.worldToLocal(mesh.localToWorld(vertex));

                        output += 'v ' + v.x + ' ' + v.y + ' ' + v.z + '\n';

                        nbVertex++;
                    }
                }

                // uvs

		if (uvs !== undefined) {
                    
		    for (i = 0, l = uvs.count; i < l; i++, nbVertexUvs++) {

			uv.x = uvs.getX(i);
			uv.y = uvs.getY(i);

			// transform the uv to export format
			output += 'vt ' + uv.x + ' ' + uv.y + '\n';
                        
		    }

		}

		// normals

		if (normals !== undefined) {

                    // get model matrix from mesh to object
                    var matrixToObject = new THREE.Matrix4();
                    matrixToObject.multiplyMatrices(mesh.matrixWorld, inverseMatrixWorld);
                
		    var normalMatrixWorld = new THREE.Matrix3();
		    normalMatrixWorld.getNormalMatrix( matrixToObject );
                    
		    for (i = 0, l = normals.count; i < l; i++, nbNormals++) {

			normal.x = normals.getX(i);
			normal.y = normals.getY(i);
			normal.z = normals.getZ(i);

			// transfrom the normal to world space
			normal.applyMatrix3(normalMatrixWorld);

			// transform the normal to export format
			output += 'vn ' + normal.x + ' ' + normal.y + ' ' + normal.z + '\n';

		    }

		}


		// material
                
		if (material.name !== '')
		    output += 'usemtl ' + material.name + '\n';
		else
		    output += 'usemtl material' + material.id + '\n';
                
		materials[material.id] = material;

                // faces
                
		if (indices !== null) {

		    for (i = 0, l = indices.count; i < l; i += 3) {

			for (m = 0; m < 3; m++) {
                            
			    j = indices.getX(i + m) + 1;

			    face[m] = (indexVertex + j) + '/' + (uvs ? (indexVertexUvs + j) : '') + '/' + (indexNormals + j);

			}

			// transform the face to export format
			output += 'f ' + face.join(' ') + "\n";

		    }

		} else {

		    for (i = 0, l = vertices.count; i < l; i += 3) {

			for (m = 0; m < 3; m++) {

			    j = i + m + 1;
                            
			    face[m] = (indexVertex + j) + '/' + (uvs ? (indexVertexUvs + j) : '') + '/' + (indexNormals + j);

			}

			// transform the face to export format
			output += 'f ' + face.join(' ') + "\n";

		    }

		}

	    }

	    // update index
	    indexVertex += nbVertex;
	    indexVertexUvs += nbVertexUvs;
	    indexNormals += nbNormals;

	};

	object.traverseVisible( function ( child ) {

	    if ( child instanceof THREE.Mesh && child.visible ) parseMesh( child );

	} );
      	
	// mtl output
        
	var mtlOutput = '';
        
	for (var key in materials) {
            
	    var mat = materials[key];
            
	    if (mat.name !== '')
		mtlOutput += 'newmtl ' + mat.name + '\n';
	    else
		mtlOutput += 'newmtl material' + mat.id + '\n';
            
	    mtlOutput += 'Ns 10.0000\n';
	    mtlOutput += 'Ni 1.5000\n';
	    mtlOutput += 'd 1.0000\n';
	    mtlOutput += 'Tr 0.0000\n';
	    mtlOutput += 'Tf 1.0000 1.0000 1.0000\n';
	    mtlOutput += 'illum 2\n';
	    mtlOutput += 'Ka ' + mat.color.r + ' ' + mat.color.g + ' ' + mat.color.b + ' ' + '\n';
	    mtlOutput += 'Kd ' + mat.color.r + ' ' + mat.color.g + ' ' + mat.color.b + ' ' + '\n';
	    mtlOutput += 'Ks 0.0000 0.0000 0.0000\n';
	    mtlOutput += 'Ke 0.0000 0.0000 0.0000\n';
            
	    if (mat.map && mat.map instanceof THREE.Texture) {
		mtlOutput += 'map_Ka ./scene.jpg\n';
		mtlOutput += 'map_Kd ./scene.jpg\n';
	    }
            
	}

	return {
	    obj: new File([output], "scene.obj"),
	    mtl: new File([mtlOutput], "scene.mtl")
	};
};
