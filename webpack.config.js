const CopyWebpackPlugin = require('copy-webpack-plugin')

module.exports = {
  mode: 'development',

  devtool: 'eval-source-map',

  entry: {
    app: [
      './src/index.js'
    ]
  },

  devServer: {
    contentBase: 'public'
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
          { loader: "css-loader", options: { sourceMap: true } },
          { loader: "sass-loader", options: { sourceMap: true } }
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
