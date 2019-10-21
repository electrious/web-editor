export class Angle {
    deg: number

    constructor(deg: number) {
        this.deg = deg
    }

    get rad(): number {
        return (this.deg * Math.PI) / 180
    }

    get sin(): number {
        return Math.sin(this.rad)
    }

    get cos(): number {
        return Math.cos(this.rad)
    }

    get tan(): number {
        return Math.tan(this.rad)
    }

    add(angle: Angle): Angle {
        return new Angle(this.deg + angle.deg)
    }

    sub(angle: Angle): Angle {
        return new Angle(this.deg - angle.deg)
    }
}
