var ExtractTextPlugin = require('extract-text-webpack-plugin');
var path = require('path');
var webpack = require('webpack');
var HtmlWebpackPlugin = require('html-webpack-plugin');

//require('bootstrap-loader');

var port = process.env.PORT || 3005;

var config = {
  entry: [
    //'bootstrap-loader',
    'webpack-hot-middleware/client?reload=true',
    path.join(__dirname, 'support/index.js'),
  ],
  debug: false,
  devtool: 'cheap-module-eval-source-map',
  output: {
    //path: path.resolve('./static'),
    path:path.join(__dirname, 'static'),
    filename: '[name].js',
    publicPath: '/'
  },
  module: {
    loaders: [
      { test: /\.js$/, loader: 'source-map-loader', exclude: /node_modules|bower_components/ },
      { test: /\.scss$/,
        //loaders: ["style", "css", "sass"]
        include:"./assets/sass/",
        loader: ExtractTextPlugin.extract('style','css!sass')
      },
      { test: /\.woff(\?v=\d+\.\d+\.\d+)?$/,   loader: "url?limit=10000&mimetype=application/font-woff" },
      { test: /\.woff2(\?v=\d+\.\d+\.\d+)?$/,  loader: "url?limit=10000&mimetype=application/font-woff" },
      { test: /\.ttf(\?v=\d+\.\d+\.\d+)?$/,    loader: "url?limit=10000&mimetype=application/octet-stream" },
      { test: /\.eot(\?v=\d+\.\d+\.\d+)?$/,    loader: "file" },
      { test: /\.svg(\?v=\d+\.\d+\.\d+)?$/,    loader: "url?limit=10000&mimetype=image/svg+xml" },
      { test: /\.purs$/,                       loader: 'purs-loader',
              exclude: /node_modules/, query: { psc: 'psa', pscArgs: { sourceMaps: true } } }
    ],
  },
  //node: { /* workaround for webpack incompatibility with ws https://github.com/websockets/ws/issues/662 */
  //  fs: 'empty',
  //  tls: 'empty'
  //},
  sassLoader: {
    includePaths: [ path.resolve(__dirname, "assets/sass")
                  , path.resolve(__dirname, "bower_components/open-iconic/font/css") ]
  }, 
  plugins: [
    new webpack.DefinePlugin({
      'process.env.NODE_ENV': JSON.stringify('development')
    }),
    new webpack.optimize.OccurenceOrderPlugin(true),
    new webpack.SourceMapDevToolPlugin({
      filename: '[file].map',
      moduleFilenameTemplate: '[absolute-resource-path]',
      fallbackModuleFilenameTemplate: '[absolute-resource-path]'
    }),
    new HtmlWebpackPlugin({
      template: 'support/index.html',
      inject: 'body',
      filename: 'index.html'
    }),
    new webpack.HotModuleReplacementPlugin(),
    new webpack.NoErrorsPlugin(),
    new ExtractTextPlugin('[name].css')
  ],
  resolveLoader: {
    root: path.join(__dirname, 'node_modules/bootstrap/bootstrap.scss')
  },
  resolve: {
    root: './node_modules',
    modulesDirectories: [
      'node_modules',
      'bower_components'
    ],
    extensions: ['', '.js', '.purs']
  },
  //externals: {
  //  ws: 'WebSocket'
  //}
};

// If this file is directly run with node, start the development server
// instead of exporting the webpack config.
if (require.main === module) {
  var compiler = webpack(config);
  var express = require('express');
  var app = express();

  // Use webpack-dev-middleware and webpack-hot-middleware instead of
  // webpack-dev-server, because webpack-hot-middleware provides more reliable
  // HMR behavior, and an in-browser overlay that displays build errors
  app
    .use(express.static('./static'))
    .use(require('connect-history-api-fallback')())
    .use(require("webpack-dev-middleware")(compiler, {
      publicPath: config.output.publicPath,
      headers: { "Access-Control-Allow-Origin": "http://localhost:3000"},
      stats: {
        hash: false,
        timings: false,
        version: false,
        assets: false,
        errors: true,
        colors: false,
        chunks: false,
        children: false,
        cached: false,
        modules: false,
        chunkModules: false,
      },
    }))
    .use(require("webpack-hot-middleware")(compiler))
    .listen(port);
} else {
  module.exports = config;
}
