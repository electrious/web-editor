import { createEditor } from '../src/Editor.purs'
import { testRoofs } from './testroofplates'

const parent = document.querySelector('#editor')

if (parent != null) {
    // create editor instance
    const editor = createEditor(800)(600)(parent)()
    // convert test data to RoofPlate objects
    const roofs = testRoofs.roofplates
    // load the house and roofs
    editor.loadHouse('https://s3.eu-west-1.amazonaws.com/data.electrious.com')(296285)(roofs)(console.log)()
}
