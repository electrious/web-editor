const path = require('path')

module.exports = {
    entry: './src/editor.ts',
    output: {
        path: path.resolve(__dirname, 'dist'),
        filename: 'web-editor.js',
        library: 'webEditor',
        libraryTarget: 'umd'
    },
    module: {
        rules: [
            {
                test: /\.ts$/,
                exclude: /node_modules/,
                loader: 'ts-loader'
            }
        ]
    },

    resolve: {
        extensions: ['.ts', '.js']
    },

    stats: {
        colors: true
    },
    devtool: 'source-map'
}
