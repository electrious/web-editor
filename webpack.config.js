 var path = require('path');
 var webpack = require('webpack');

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
        }]
     },
     resolve: {
         extensions: ['.ts', '.js']
     },
     stats: {
         colors: true
     },
     devtool: 'source-map'
 };