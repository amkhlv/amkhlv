#lang setup/infotab
(define collection 'use-pkg-name)
(define version "9.0")
(define deps 
  '("base"
    "compatibility-lib"
    "db-lib"
    "scheme-lib"
    "scribble-lib"))
(define build-deps '("racket-doc"
                     "rackunit-lib"
                     "scribble-doc"
                     "at-exp-lib"))
(define scribblings
    '(("manual.scrbl" ()))) 
