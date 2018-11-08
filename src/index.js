'use strict';

import 'lazysizes';
require('./index.html');
require('./main.scss');

var Elm = require('./Main.elm');
var mountNode = document.getElementById('elm-app');

var app = Elm.Elm.Main.init({
  node: mountNode,
  flags: {
    rootUrl: "https://iiif.durham.ac.uk/manifests/trifle/collection/32150/t3cvx021f09g"
  }
});

