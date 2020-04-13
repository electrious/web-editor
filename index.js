const editor = require('./output/Editor.Editor/index')

// Wrap the purescript API in a idiomatic JS API
module.exports = {
    createEditor: (width, height, elem) => {
        const e = editor.createEditor(width)(height)(elem)()

        return {
            resize: (width, height) => { e.resize(width)(height)() },
            dispose: e.dispose,
            loadHouse: (url, leadId, roofs, roofEdited) => {
                let f = newRoofs => {
                    return _ => {
                        roofEdited(newRoofs)
                    }
                }

                e.loadHouse(url)(leadId)(roofs)(f)()
            }
        }
    }
}
