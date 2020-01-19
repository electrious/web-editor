import { createEditor } from '../src/editor'
import { testRoofs } from './testroofplates'

const parent = document.querySelector('#editor')

if (parent != null) {
    // create editor instance
    const editor = createEditor(800, 600, parent)
    // convert test data to RoofPlate objects
    const roofs = testRoofs.roofplates
    // load the house and roofs
    editor.loadHouse(296285, roofs, r => {
        // updated roof
        console.log(r)
    })
}
