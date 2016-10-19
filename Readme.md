
# Wolfram Document Viewer

This is a package I wrote in a day or two to facilitate previews of notebooks on Github by creating a badge which links out to the Wolfram Cloud.

### Installing

The code depends upon having an active Wolfram Cloud account that you are logged into and have write access to.  Also, the code bootstraps itself within a desktop front end, so you'll need a copy of Wolfram Desktop or Mathematica to try it out.  To install, open WolframDocViewer.wl in your desktop front end and follow the instructions.

### About

It's not production quality.  It doesn't handle errors well, for example.  But it's not very long, and I think it's a good example of how to write useful API functions which interact with notebooks and other web APIs.  The idea could be trivially extended to allow previews for other content providers with REST APIs.
