const path = require('path')
const HtmlWebPackPlugin = require('html-webpack-plugin')

module.exports = {
    entry: './example/index.ts',
    output: {
        path: path.resolve(__dirname, 'dist'),
        filename: 'demo.bundle.js'
    },
    module: {
        rules: [
            {
                test: /\.ts$/,
                exclude: /node_modules/,
                loader: 'ts-loader'
            },
            {
                test: /\.html$/,
                exclude: /node_modules/,
                loader: 'html-loader'
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
        extensions: ['.ts', '.js', '.html', '.purs']
    },
    stats: {
        colors: true
    },
    devtool: 'source-map',
    plugins: [
        new HtmlWebPackPlugin({
            template: './example/index.html',
            filename: './index.html'
        })
    ]
}
