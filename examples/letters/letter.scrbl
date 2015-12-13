#lang scribble/base
@(require racket scribble/core scribble/base scribble/html-properties)
@(require "defs.rkt" bystroTeX/common bystroTeX/slides (for-syntax bystroTeX/slides_for-syntax))
@; ---------------------------------------------------------------------------------------------------
@; User definitions:
@(bystro-set-css-dir (build-path 'same "css"))
@(define dst (bystro-get-cl-argument "dest"))
@(define to-whom (bystro-get-cl-argument "to_whom"))
@(define bystro-conf 
   (bystro (bystro-connect-to-server #f "127.0.0.1" 9749 "svg") ;(find-executable-path "amkhlv-java-formula.sh")
           "formulas.sqlite"  ; name for the database
           dst ; directory where to store the image files of formulas
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

@; AND HOPEFULLY SOME CONTENT:


@; @(define to-whom
@;    (let iter ([arglist (vector->list (current-command-line-arguments))])
@;      (if (equal? (car arglist) "--to_whom") 
@;          (cadr arglist)
@;          (iter (cdr arglist)))))



@title[#:style '(no-toc no-sidebar)]{Dear @(elem to-whom)! }

please read the enclosed text presenting my idea about the interpretation of the Higgs boson as a wormhole in graphene-lattice.


@; ---------------------------------------------------------------------------------------------------
@(bystro-close-connection bystro-conf)
@disconnect[formula-database]

  
