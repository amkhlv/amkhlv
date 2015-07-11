#lang scribble/base
@(require racket scribble/core scribble/base scribble/html-properties)
@(require "defs_for-syntax.rkt" (for-syntax bystroTeX/slides_for-syntax))
@(require "defs.rkt" bystroTeX/common bystroTeX/slides)
@; ---------------------------------------------------------------------------------------------------
@; User definitions:
@(bystro-set-css-dir (build-path 'same "css"))
@(define bystro-conf   
   (bystro (bystro-connect-to-server #f "127.0.0.1" 9749 "svg")
           "sample-plain_formulas.sqlite"  ; name for the database
           "sample-plain" ; directory where to store .png files of formulas
           25  ; formula size
           (list 255 255 255) ; formula background color
           (list 0 0 0) ; formula foreground color
           2   ; automatic alignment adjustment
           0   ; manual alignment adjustment
           ))
@(set-bystro-extension! bystro-conf "svg")
@; This controls the single page mode:
@(define singlepage-mode #t)
@(bystro-def-formula "formula-enormula-humongula!")

@(bystro-inject-style "misc.css" "no-margin.css")

@title[#:style '(no-toc no-sidebar)]{Plain}

@bystro-ribbon[]

plain scribble file with a formula: @f{y=x^2}

@; ---------------------------------------------------------------------------------------------------
@(bystro-close-connection bystro-conf)
@disconnect[formula-database]

  
