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
      if (data["value"]["url"] == "") viewer.close();
      else {
        if(data["value"]["sourceType"] == "image") {
          viewer.open({
            type: 'image',
            url: data["value"]["url"]
          });
        }
        else if(data["value"]["sourceType"] == "iiif") {
          viewer.open(data["value"]["url"]);
        }
        else {
          viewer.close();
        }
      }
    }
  });

  app.ports.outPortScrollToView.subscribe(function (data) {
    // requestAnimationFrame forces dom updates to happen first
    requestAnimationFrame(function () {
      var container = $(data["container"]);
      var item = null;
      if (data["ref"]) item = container.find(data["ref"]);

      if ((item != null && item.length > 0) || data["pos"] != null) {
        if (data["axis"] == "x") {
          var xpos = (item == null ? data["pos"] : (item.offset().left + container.scrollLeft()) );
          var scrollPos = xpos - container.width() / 2.0 + (item == null ? 0 : item.width()) / 2.0;
          if (data["alignment"] == "start") scrollPos = xpos;
          if (data["animate"]) container.animate({ scrollLeft: scrollPos });
          else container.scrollLeft(scrollPos)
        }
        else {
          var ypos = (item == null ? data["pos"] : (item.offset().top + container.scrollTop()) );
          var scrollPos = ypos - container.height() / 2.0 + (item == null ? 0 : item.height()) / 2.0;
          if (data["alignment"] == "start") scrollPos = ypos;
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