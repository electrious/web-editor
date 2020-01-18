export declare const testRoofs: {
    "roofplates": ({
        "id": number;
        "uuid": string;
        "created_at": string;
        "lead_id": number;
        "border_points": {
            "x": number;
            "y": number;
            "z": number;
        }[];
        "unified_points": {
            "x": number;
            "y": number;
            "z": number;
            "shade": number;
            "rating": number;
        }[];
        "orientation": number;
        "alignment": number;
        "slope": number;
        "coefs": number[];
        "center": number[];
        "normal": number[];
        "azimuth": number;
        "rotation_override": number;
        "setbacks"?: undefined;
    } | {
        "id": number;
        "uuid": string;
        "created_at": string;
        "lead_id": number;
        "border_points": {
            "x": number;
            "y": number;
            "z": number;
        }[];
        "unified_points": {
            "x": number;
            "y": number;
            "z": number;
            "shade": number;
            "rating": number;
        }[];
        "orientation": number;
        "alignment": number;
        "slope": number;
        "coefs": number[];
        "center": number[];
        "normal": number[];
        "azimuth": number;
        "rotation_override": number;
        "setbacks": {
            "min_ridge_setback": {
                "x": number;
                "y": number;
                "z": number;
            }[];
            "min_ridge_setback_midpoints": {
                "x": number;
                "y": number;
                "z": number;
            }[];
            "min_side_setback": {
                "x": number;
                "y": number;
                "z": number;
            }[];
            "min_side_setback_midpoints": {
                "x": number;
                "y": number;
                "z": number;
            }[];
            "max_ridge_setback": {
                "x": number;
                "y": number;
                "z": number;
            }[];
            "max_ridge_setback_midpoints": {
                "x": number;
                "y": number;
                "z": number;
            }[];
            "max_side_setback": {
                "x": number;
                "y": number;
                "z": number;
            }[];
            "max_side_setback_midpoints": {
                "x": number;
                "y": number;
                "z": number;
            }[];
        };
    })[];
};
