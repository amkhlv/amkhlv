#lang setup/infotab
(define collection 'use-pkg-name)
(define version "9.1")
(define deps 
  '("base"
    "compatibility-lib"
    "db-lib"
    "scheme-lib"
    "scribble-lib"))
(define build-deps '("net-doc"
                     "racket-doc"
                     "rackunit-lib"
                     "scribble-doc"
                     "at-exp-lib"))
(define scribblings
    '(("manual.scrbl" ()))) 
