const CopyWebpackPlugin = require('copy-webpack-plugin')
const MiniCssExtractPlugin = require('mini-css-extract-plugin')

module.exports = {
  mode: 'production',

  entry: {
    jalava: [
      './src/index.js', 
      './src/main.scss'
    ]
  },

  plugins : [
    new CopyWebpackPlugin([{from: 'node_modules/openseadragon/build/openseadragon/images', to :'osd/'}]),
    new CopyWebpackPlugin([{from: 'src/images', to :'images/'}]),
    new MiniCssExtractPlugin()
  ],

  module: {
    rules: [
      {
        test: /\.scss$/,
        use: [
          { loader: "file-loader", options : { name: '[name].css' } },
          MiniCssExtractPlugin.loader,
          { loader: "css-loader?-url" },
          { loader: "sass-loader" }
        ]
      },
/*      {
          test: /\.(png|jpg|jpeg|gif)$/,
          use : [ { loader: 'file-loader' } ]
      },*/
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
