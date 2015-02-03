#!/bin/bash

rm -rf compiled
rm  manual.html
rm  *.css
rm  scribble-common.js
rm -rf example-slides/formulas/
rm  example-slides/formulas.sqlite
rm  example-slides/bystrotex.fifo
rm  example-slides/server-log.txt
rm  example-slides/server-error.txt
rm -rf example-slides/*.png
rm -rf example-slides/*.svg
rm -rf example-slides/*.html
rm -rf example-slides/slides-manual/
rm planet-docs/manual/*
rm doc/manual/*
find . -type f -name '*~' -delete
