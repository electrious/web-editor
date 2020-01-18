export declare class Angle {
    deg: number;
    constructor(deg: number);
    static fromRad(rad: number): Angle;
    get rad(): number;
    get sin(): number;
    get cos(): number;
    get tan(): number;
    add(angle: Angle): Angle;
    sub(angle: Angle): Angle;
}
