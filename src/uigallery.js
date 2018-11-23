'use strict';

require('./uigallery.html');

var Main = require('./UIGallery.elm');
var mountNode = document.getElementById('elm-app');

var app = Main.Elm.UIGallery.init({
  node: mountNode
});
