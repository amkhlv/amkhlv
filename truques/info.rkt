#lang setup/infotab
(define collection 'use-pkg-name)
(define version "1.0")
(define deps 
  '("base"
    "db-lib"
    "scheme-lib"
    "scribble-lib"))
(define build-deps '("at-exp-lib"))
(define scribblings
    '(("manual.scrbl" ()))) 
