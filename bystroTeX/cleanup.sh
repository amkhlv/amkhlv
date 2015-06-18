#!/bin/bash

rm -rf compiled
rm  manual.html
rm  *.css
rm  scribble-common.js
rm planet-docs/manual/*
rm doc/manual/*
find . -type f -name '*~' -delete
