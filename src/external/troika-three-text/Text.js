const troika = require('troika-three-text');

exports.mkText = _ => {
    return new troika.Text();
};

exports.setText = s => t => _ => {
    t.text = s;
    t.position.z = 0.01;
};

exports.setFontSize = s => t => _ => {
    t.fontSize = s;
};

exports.setColor = c => t => _ => {
    t.color = c;
};

exports.setTextAlign = a => t => _ => {
    t.textAlign = a;
};

exports.sync = t => _ => {
    t.sync();
};

exports.dispose = t => _ => {
    t.dispose();
};
