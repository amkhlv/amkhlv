#lang scribble/base
@(require racket scribble/core scribble/base scribble/decode scribble/html-properties racket/port)
@(require "defs.rkt" bystroTeX/common bystroTeX/slides bystroTeX/utils (for-syntax bystroTeX/slides_for-syntax) (for-syntax bystroTeX/utils))
@; ---------------------------------------------------------------------------------------------------
@; User definitions:
@(bystro-set-css-dir (build-path (find-system-path 'home-dir) "a" "git" "amkhlv" "profiles" "writeup"))
@(define bystro-conf   
   (bystro (bystro-connect-to-server #f)
           "lookdown/formulas.sqlite"  ; name for the database
           "lookdown" ; directory where to store .png files of formulas
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

@(define bystrotex-xml-files
   (with-external-command-as
    finder
    #:cmdline
    '("find" "." "-type" "f" "-name" "bystrotex.xml")
    (port->lines finder-stdout)))
   
@(define (dirname x)
   (with-external-command-as
    dirname
    #:cmdline
    `("dirname" ,x)
    (read-line dirname-stdout)))

@(define (get-lookdown.html x)
   (let* ([xdir (dirname x)]
          [lookdown.html (build-path xdir "lookdown/lookdown.html")])
     (if (file-exists? lookdown.html)
         lookdown.html
         #f)))

@title[#:style '(no-toc no-sidebar)]{@(path->string (current-directory))}

@(decode
  (cons
   (title-decl 
    #f 
    '((part "LookdownMain"))
    #f
    (make-style "LookdownMainTitle" '(unnumbered))
    "")
   `(
     ,(part
       #f
       '((part "Lookdown"))
       '()
       (make-style "LookdownPart" '(unnumbered))
       '()
       `(,(bystro-ribbon)
         ,(tbl #:orient 'hor
               (for/list ([bystrotex.xml bystrotex-xml-files])
                 (list
                  (let ([lookdown.html (get-lookdown.html bystrotex.xml)])
                    (if lookdown.html (hyperlink lookdown.html "lookdown") ""))
                  (seclink (dirname bystrotex.xml))))))
       '())
     ,@(for/list ([bystrotex.xml bystrotex-xml-files])
         (part 
          #f 
          (list (list 'part (dirname bystrotex.xml))) 
          `(,(dirname bystrotex.xml)) 
          (make-style "LookdownPartFolder" '(unnumbered))
          '()
          (begin
                                        ;(displayln bystrotex.xml)
            (list (bystro-ribbon-for-location (string->path (dirname bystrotex.xml)))))
          '())))))

@; ---------------------------------------------------------------------------------------------------
@(bystro-close-connection bystro-conf)
@disconnect[formula-database]
