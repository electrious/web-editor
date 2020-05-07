const path = require('path')
const HtmlWebPackPlugin = require('html-webpack-plugin')

module.exports = {
    entry: './example/index.js',
    output: {
        path: path.resolve(__dirname, 'dist'),
        filename: 'demo.bundle.js'
    },
    module: {
        rules: [
            {
                test: /\.html$/,
                exclude: /node_modules/,
                loader: 'html-loader'
            },
            {
                test: /\.(jpg|png)$/,
                use: ['file-loader']
            },
            {
                test: /\.purs$/,
                exclude: /node_modules/,
                loader: 'purs-loader',
                query: {
                    psc: 'psa',
                    spago: true,
                    src: ['src/**/*.purs', 'example/**/*.purs']
                }
            }
        ]
    },
    resolve: {
        extensions: ['.js', '.html', '.purs', '.jpg', 'png']
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
