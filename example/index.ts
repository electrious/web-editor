import { createEditor } from '../src/editor'

const parent = document.querySelector('#editor')

if (parent != null) {
    const editor = createEditor(800, 600, parent)
    editor.loadHouse(296285)
}
