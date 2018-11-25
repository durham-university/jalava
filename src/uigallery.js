'use strict';

import 'lazysizes';

require('./uigallery.html');

window.lazySizesConfig = window.lazySizesConfig || {};
window.lazySizesConfig.loadHidden = false;

var Main = require('./UIGallery.elm');
var mountNode = document.getElementById('elm-app');

var app = Main.Elm.UIGallery.init({
  node: mountNode
});
