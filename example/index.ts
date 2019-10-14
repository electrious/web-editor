import { createEditor } from "../src/editor";

let parent = document.querySelector("#editor");
if (parent != null) {
    createEditor(800, 600, parent!);
}