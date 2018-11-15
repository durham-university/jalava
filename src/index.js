'use strict';

import 'lazysizes';

var OpenSeadragon = require('openseadragon');
require('./index.html');
require('./main.scss');

window.lazySizesConfig = window.lazySizesConfig || {};
window.lazySizesConfig.loadHidden = false;

var Elm = require('./Main.elm');
var mountNode = document.getElementById('elm-app');

var app = Elm.Elm.Main.init({
  node: mountNode,
  flags: {
//    rootUrl: "https://iiif.durham.ac.uk/manifests/trifle/collection/32150/t3cvx021f09g",
    rootUrl: "http://ryanfb.github.io/iiif-universe/iiif-universe.json",
    uriMapper: {
      inflaters: [
        { regex: "^(t\\d)(m[a-z0-9])([a-z0-9]{2})([a-z0-9]+)$", 
          pattern: "https://iiif.durham.ac.uk/manifests/trifle/32150/$1/$2/$3/$1$2$3$4/manifest"},
        { regex: "^(t\\d)(m[a-z0-9])([a-z0-9]{2})([a-z0-9]+)_(t\\dt[a-z0-9]+)$", 
          pattern: "https://iiif.durham.ac.uk/manifests/trifle/32150/$1/$2/$3/$1$2$3$4/canvas/$5"},
        { regex: "^(t\\dc[a-z0-9]+)$", 
          pattern: "https://iiif.durham.ac.uk/manifests/trifle/collection/32150/$1"}
      ],
      deflaters: [
        { regex: "^https://iiif.durham.ac.uk/manifests/trifle/32150/[a-z0-9]{2}/[a-z0-9]{2}/[a-z0-9]{2}/([a-z0-9]+)/manifest$",
          pattern: "$1"},
        { regex: "^https://iiif.durham.ac.uk/manifests/trifle/32150/[a-z0-9]{2}/[a-z0-9]{2}/[a-z0-9]{2}/([a-z0-9]+)/canvas/([a-z0-9]+)$",
          pattern: "$1_$2"},
        { regex: "^https://iiif.durham.ac.uk/manifests/trifle/collection/32150/([a-z0-9]+)$",
          pattern: "$1"}
      ]
    }
  }
});

var osdViewers = {};

app.ports.osdCmd.subscribe(function (data) {
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

app.ports.scrollToView.subscribe(function (data) {
  // requestAnimationFrame forces dom updates to happen first
  requestAnimationFrame(function(){
    var container = $(data["container"]);
    var item = container.find(data["item"]);
  
    if(item.length > 0) {
      if(data["axis"] == "x"){
        var xpos = item.offset().left + container.scrollLeft();
        var scrollPos = xpos - container.width()/2.0 + item.width()/2.0;
        if(data["animate"]) container.animate({scrollLeft: scrollPos});
        else container.scrollLeft(scrollPos)
      }
      else {
        var ypos = item.offset().top + container.scrollTop();
        var scrollPos = ypos - container.height()/2.0 + item.height()/2.0;
        if(data["animate"]) container.animate({scrollTop: scrollPos});
        else container.scrollTop(scrollPos)
      }
    }  
  })
});

document.addEventListener('lazybeforeunveil', function(e){
  var elem = $(e.target);
  if(elem.hasClass("manifest_lazyload")) {
    var uri = elem.data("manifest-uri")
    if(uri) app.ports.lazyLoadManifest.send(uri);
  }
});