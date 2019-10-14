import { OBJLoader2 } from "three/examples/jsm/loaders/OBJLoader2";
import { MTLLoader } from "three/examples/jsm/loaders/MTLLoader";
import { Object3D } from "three";
import { Observable } from "rxjs";

/**
 *      
 * @param {String} obj 
 * @param {String} mtl 
 * @param {String} jpg 
 */
export let loadHouse = (obj: string, mtl: string, jpg: string) : Observable<Object3D> => {
    let objLoader = new OBJLoader2();
    let mtlLoader = new MTLLoader();

    return new Observable<Object3D>((subscriber) => {
        mtlLoader.load(mtl, (materials) => {
            materials.preload();
            
            objLoader.addMaterials(materials);
            objLoader.load(obj, (object) => {
                let childs = object.children;
                childs.forEach(c => {
                    c.castShadow = true;
                    c.receiveShadow = true;
                });

                subscriber.next(object);
                subscriber.complete();
            });
        });
    });
}
