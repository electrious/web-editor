const path = require('path')

module.exports = {
    entry: './src/index.ts',
    output: {
        path: path.resolve(__dirname, 'dist'),
        filename: 'web-editor.js',
        library: 'webEditor',
        libraryTarget: 'umd',
        umdNamedDefine: true
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

    externals: ['three',
                'ramda',
                'rbush',
                'uuid',
                '@most/adaptor',
                '@most/core',
                '@most/dom-event',
                '@most/scheduler',
                '@types/ramda',
                '@types/rbush',
                '@types/uuid'
            ],
    stats: {
        colors: true
    },
    devtool: 'source-map'
}
