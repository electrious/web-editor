import { Object3D, Vector2 } from 'three';
import { Stream, Disposable } from '@most/types';
import { SceneTapEvent } from '../sceneevent';
export interface RoofEditor {
    roofVertices: Stream<Vector2[]>;
    deleteRoof: Stream<SceneTapEvent>;
    disposable: Disposable;
}
export declare const createRoofEditor: (parent: Object3D, active: Stream<boolean>, ps: Vector2[]) => RoofEditor;
