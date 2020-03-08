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
            },
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
        extensions: ['.ts', '.js', '.purs']
    },

    stats: {
        colors: true
    },
    devtool: 'source-map'
}
