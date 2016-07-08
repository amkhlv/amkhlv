#lang scribble/base
@(require racket scribble/core scribble/base scribble/html-properties)
@(require "defs.rkt" bystroTeX/common bystroTeX/slides (for-syntax bystroTeX/slides_for-syntax))
@; ---------------------------------------------------------------------------------------------------
@; User definitions:
@(bystro-set-css-dir (build-path 'same "css"))
@(define bystro-conf   
   (bystro (bystro-connect-to-server (build-path 'same "serverconf.xml")) 
           "sample-multipage/formulas.sqlite"  ; name for the database
           "sample-multipage" ; directory where to store .png files of formulas
           25  ; formula size
           (list 255 255 255) ; formula background color
           (list 0 0 0) ; formula foreground color
           2   ; automatic alignment adjustment
           0   ; manual alignment adjustment
           ))
@(set-bystro-extension! bystro-conf "svg")
@; This controls the single page mode:
@(define singlepage-mode #f)
@(bystro-def-formula "formula-enormula-humongula!")

@(bystro-inject-style "misc.css" "no-margin.css")

@title[#:style '(no-toc no-sidebar)]{Multipage}
@bystro-ribbon[]
@table-of-contents[]

@slide["First slide" #:tag "FirstSlide" #:showtitle #t]{

this is our first slide

}


@slide["Second slide" #:tag "SecondSlide" #:showtitle #t]{

this is our second slide

}



@; ---------------------------------------------------------------------------------------------------
@(bystro-close-connection bystro-conf)
@disconnect[formula-database]

  
