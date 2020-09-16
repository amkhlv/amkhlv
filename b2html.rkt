#!/usr/bin/env racket

#lang racket

(require scribble/reader)
(require scribble/html/html)
(require scribble/html/xml)

(define/match (getHTML x)
  [((cons 'section u)) (apply h3 u)]
  [((cons 'hyperlink u)) (apply a `(href: ,(car u) ,@(map getHTML (cdr u))))]
  [((cons 'f u)) (car u)]
  [(_) x]
  )  

(for ([ln (syntax->datum (read-syntax-inside))])
  (output-xml (getHTML ln))
  )
