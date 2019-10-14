var path = require('path');
var webpack = require('webpack');
const HtmlWebPackPlugin = require("html-webpack-plugin");

module.exports = {
    entry: './src/editor.ts',
    output: {
        path: path.resolve(__dirname, 'dist'),
        filename: 'editor.bundle.js'
    },
    module: {
        rules: [{
            test: /\.ts$/,
            exclude: /node_modules/,
            loader: "ts-loader"
        },
        {
            test: /\.html$/,
            exclude: /node_modules/,
            loader: "html-loader"
        }]
    },
    resolve: {
        extensions: ['.ts', '.js', '.html']
    },
    stats: {
        colors: true
    },
    devtool: 'source-map',
    plugins: [
        new HtmlWebPackPlugin({
            template: "./example/index.html",
            filename: "./index.html"
        })
    ]
};