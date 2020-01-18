import { Object3D } from "three";
import { Observable } from "rxjs";
/**
 *
 * @param {String} obj
 * @param {String} mtl
 * @param {String} jpg
 */
export declare let loadHouse: (obj: string, mtl: string, jpg: string) => Observable<Object3D>;
