'use strict';

import 'lazysizes';
var OpenSeadragon = require('openseadragon');
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

var osdViewers = {};

app.ports.osdCmd.subscribe(function (data) {
  console.log("osdCmd " + JSON.stringify(data));

  if (data["type"] == "setSource") {
    var viewerId = data["for"];
    var viewerElem = document.getElementById(viewerId);
    var viewer = null;
    if (viewerElem.childElementCount == 0) {
      viewer = OpenSeadragon({
        id: viewerId,
        prefixUrl: "osd/"
      })
      osdViewers[viewerId] = viewer;
    }
    else {
      viewer = osdViewers[viewerId];
    }

    if(data["value"] == "") viewer.close();
    else viewer.open(data["value"]);
  }
});