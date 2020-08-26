const path = require('path');

module.exports = {
entry: './main.js',
output: {
    path: path.resolve(__dirname, 'dist'),
    filename: 'main.js'
},
module: {
    rules: [
        {
            test: /\.js$/, //using regex to tell babel exactly what files to transcompile
            exclude: /node_modules/, // files to be ignored
            use: {
                loader: 'babel-loader' // specify the loader
            }
        }
    ]
}
}
