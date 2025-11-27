const CopyWebpackPlugin = require('copy-webpack-plugin')
const MiniCssExtractPlugin = require('mini-css-extract-plugin')

module.exports = {
  mode: 'development',

  devtool: 'eval-source-map',

  watch: true,

  entry: {
    jalava: ['./src/index.js', './src/main.scss'],
    uigallery: ['./src/uigallery.js', './src/main.scss']
  },

  output : {
    filename: '[name].js'
  },

  devServer: {
//    host: '0.0.0.0',
    contentBase: 'public'
  },

  plugins : [
    new CopyWebpackPlugin([{from: 'node_modules/openseadragon/build/openseadragon/images', to :'osd/'}]),
    new MiniCssExtractPlugin()
  ],

  module: {
    rules: [
      {
        test: /\.scss$/,
        use: [
          { loader: "file-loader", options : { name: '[name].css' } },
          MiniCssExtractPlugin.loader,
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
          options: {
            pathToElm: './node_modules/.bin/elm'
          }
        }
      }
    ]
  }
};
