const CopyWebpackPlugin = require('copy-webpack-plugin')

module.exports = {
  mode: 'production',

  entry: {
    app: [
      './src/index.js'
    ]
  },

  plugins : [
    new CopyWebpackPlugin([{from: 'node_modules/openseadragon/build/openseadragon/images', to :'osd/'}])
  ],

  module: {
    rules: [
      {
        test: /\.scss$/,
        use: [
          { loader: "style-loader" },
          { loader: "css-loader" },
          { loader: "sass-loader" }
        ]
      },
      {
          test: /\.(png|jpg|jpeg|gif)$/,
          use : [ { loader: 'file-loader' } ]
      },
      {
        test: /\.html$/,
        exclude: /node_modules/,
        loader: 'file-loader?name=[name].[ext]'
      },
      {
        test: /\.elm$/,
        exclude: [/elm-stuff/, /node_modules/],
        use: {
          loader: 'elm-webpack-loader',
          options: {}
        }
      }
    ]
  }
};
