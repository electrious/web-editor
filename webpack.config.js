const path = require('path')

module.exports = {
    entry: './src/Editor.purs',
    output: {
        path: path.resolve(__dirname, 'dist'),
        filename: 'web-editor.js',
        library: 'webEditor',
        libraryTarget: 'umd'
    },
    module: {
        rules: [
            {
                test: /\.purs$/,
                exclude: /node_modules/,
                loader: 'purs-loader',
                query: {
                    psc: 'psa',
                    spago: true,
                    src: ['src/**/*.purs']
                }
            }
        ]
    },

    resolve: {
        extensions: ['.js', '.purs']
    },

    stats: {
        colors: true
    },
    devtool: 'source-map'
}
