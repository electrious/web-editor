"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
var editor_1 = require("../src/editor");
var testroofplates_1 = require("./testroofplates");
var parent = document.querySelector('#editor');
if (parent != null) {
    // create editor instance
    var editor = editor_1.createEditor(800, 600, parent);
    // convert test data to RoofPlate objects
    var roofs = testroofplates_1.testRoofs.roofplates;
    // load the house and roofs
    editor.loadHouse('https://s3.eu-west-1.amazonaws.com/data.electrious.com', 296285, roofs, function (r) {
        // updated roof
        console.log(r);
    });
}
//# sourceMappingURL=index.js.map