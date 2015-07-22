#lang scribble/base
@(require racket scribble/core scribble/base scribble/html-properties)
@(require "defs_for-syntax.rkt" (for-syntax bystroTeX/slides_for-syntax))
@(require "defs.rkt" bystroTeX/common bystroTeX/slides)
@; ---------------------------------------------------------------------------------------------------
@; User definitions:
@(bystro-set-css-dir (build-path 'same "css"))
@(define bystro-conf 
   (bystro (bystro-connect-to-server #f "127.0.0.1" 9749 "svg") ;(find-executable-path "amkhlv-java-formula.sh")
           "show-xml/formulas.sqlite"  ; name for the database
           "show-xml" ; directory where to store the image files of formulas
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
@(require truques/xml)
@title[#:style '(no-toc no-sidebar)]{Show XML}

@table-of-contents[]
@bystro-ribbon[]

@section{String}

@(show-xexpr "this is a string")

@section{Symbol}
@(show-xexpr 'symbol-example)

@section{Nodes}
@subsection{With string value}
one value:
@(show-xexpr '(sample-node "hi"))
two values:
@(show-xexpr '(sample-node "line 1" "line 2"))
tree:
@(show-xexpr '(tree (node1 "line 1" "line 2") (node2 "line 1" "line 2")))

@section{Nodes with attributes}
@(show-xexpr '(with-attributes ((a "attr-a") (b "attr-b")) "string value"))

@section{XML from file}

@(let ([href (λ (x) (nested (hyperlink (se-path* '(a #:href) x) (se-path* '(a) x))))])
   (show-xexpr 
    (file->xexpr "example.xml")
    #:transformers (hash 'a href)))



@; ---------------------------------------------------------------------------------------------------
@(bystro-close-connection bystro-conf)
@disconnect[formula-database]

  