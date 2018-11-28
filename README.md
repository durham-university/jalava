Jalava
======

[Elm](https://elm-lang.org/) based [IIIF](https://iiif.io/) viewer prototype. See [live demo](http://iiif.durham.ac.uk/jalava/universe.html).

IIIF is a standardised way of describing image repositories and a way to access the images in them. Jalava provides a simple directory tree based discovery interface for IIIF compliant repositories.

The default starting point for browsing is provided by [IIIF-Universe](https://github.com/ryanfb/iiif-universe) which collects IIIF repositories from various institutitons around the world. This way Jalava gives you easy access to a huge number of items from many different digital libraries.

At the time of writing this, a few of the endpoints in IIIF-Universe don't appear to work. Some others may use features (such as paging) that Jalava does not yet support. If you get error messages or apparently empty repositories, just try another one.

Note that the image material you find in the application is provided by various institutions over public APIs. Licensing and copyright for these vary. See the information tab for each individual item for details.

Development
===========

To run Jalava locally, first clone the repository and then run

```
npm install
node_modules/.bin/webpack-dev-server
```

And then go to http://localhost:8080.

License
=======

Jalava is licensed under [the MIT license](LICENSE).
