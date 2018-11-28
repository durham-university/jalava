'use strict';

import 'lazysizes';

window.OpenSeadragon = require('openseadragon');
require('./index.html');
require('./main.scss');

window.lazySizesConfig = window.lazySizesConfig || {};
window.lazySizesConfig.loadHidden = false;

window.mountJalava = function(mountNode, config){
  var Main = require('./Main.elm');
  var AnnotationOverlay = require('./AnnotationOverlay.elm');

  var app = Main.Elm.Main.init({
    node: mountNode,
    flags: Object.assign({
      rootUrl: "http://ryanfb.github.io/iiif-universe/iiif-universe.json",
      uriMapper: {
        inflaters: [],
        deflaters: []
      }
    }, config || {})
  });
  var overlayApp = null;

  app.ports.outPortOsdCmd.subscribe(function (data) {
    var viewerId = data["for"];
    var viewerElem = $("#" + viewerId);
    var viewer = viewerElem.data('osdviewer');
    if (!viewer) {
      viewer = OpenSeadragon({
        id: viewerId,
        prefixUrl: "osd/",
        preserveOverlays: true
      })

      viewer.addOverlay({
        element: document.getElementById("annotation_overlay_wrapper"),
        location: new OpenSeadragon.Rect(0.0, 0.0, 1.0, 1.0),
        checkResize: true
      });

      overlayApp = AnnotationOverlay.Elm.AnnotationOverlay.init({
        node: document.getElementById("annotation_overlay"),
        flags: null
      });  

      app.ports.outPortSetAnnotationsCmd.subscribe(function(data){
        overlayApp.ports.inPortSetAnnotations.send(data);
      });

      overlayApp.ports.outPortShowAnnotation.subscribe(function(data){
        app.ports.inPortShowAnnotation.send(data);
      });

      viewerElem.data('osdviewer', viewer);
    }

    if (data["type"] == "setSource") {
      if (data["value"] == "") viewer.close();
      else {
        viewer.open(data["value"]);
      }
    }
  });

  app.ports.outPortScrollToView.subscribe(function (data) {
    // requestAnimationFrame forces dom updates to happen first
    requestAnimationFrame(function () {
      var container = $(data["container"]);
      var item = container.find(data["item"]);

      if (item.length > 0) {
        if (data["axis"] == "x") {
          var xpos = item.offset().left + container.scrollLeft();
          var scrollPos = xpos - container.width() / 2.0 + item.width() / 2.0;
          if (data["animate"]) container.animate({ scrollLeft: scrollPos });
          else container.scrollLeft(scrollPos)
        }
        else {
          var ypos = item.offset().top + container.scrollTop();
          var scrollPos = ypos - container.height() / 2.0 + item.height() / 2.0;
          if (data["animate"]) container.animate({ scrollTop: scrollPos });
          else container.scrollTop(scrollPos)
        }
      }
    })
  });

  document.addEventListener('lazybeforeunveil', function (e) {
    var elem = $(e.target);
    if (elem.hasClass("manifest_lazyload")) {
      var uri = elem.data("manifest-uri")
      if (uri) app.ports.inPortLazyLoadManifest.send(uri);
    }
  });
};