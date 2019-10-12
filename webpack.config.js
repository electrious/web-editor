 var path = require('path');
 var webpack = require('webpack');

 module.exports = {
     mode: "development",
     entry: './src/editor.js',
     output: {
         path: path.resolve(__dirname, 'build'),
         filename: 'editor.bundle.js'
     },
     module: {
        rules: [{
           test: /\.js$/,
           exclude: /node_modules/,
           loader: "babel-loader"
        }]
     },
     stats: {
         colors: true
     },
     devtool: 'source-map'
 };