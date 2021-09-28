#lang scribble/base
@(require racket scribble/core scribble/base scribble/decode scribble/html-properties racket/port racket/string)
@(require "defs.rkt" bystroTeX/common bystroTeX/slides bystroTeX/utils (for-syntax bystroTeX/slides_for-syntax) (for-syntax bystroTeX/utils))
@; ---------------------------------------------------------------------------------------------------
@; User definitions:
@(bystro-set-css-dir (build-path (find-system-path 'home-dir) "a" "git" "amkhlv" "profiles" "writeup"))
@(define bystro-conf   
   (bystro (bystro-connect-to-server #f)
           "lookdown/formulas.sqlite"  ; name for the database
           "lookdown" ; directory where to store image files of formulas
           25  ; formula size
           (list 255 255 255) ; formula background color
           (list 0 0 0) ; formula foreground color
           2   ; automatic alignment adjustment
           0   ; manual alignment adjustment
           ))
@(define singlepage-mode #t)
@(bystro-def-formula "formula-enormula-humongula!")

@(define bystrotex.xml-dirs
   (let* ([inp (run-pipeline #f #f ("find" "." "-type" "f" "-name" "bystrotex.xml") ("sort"))]
          [lns (port->lines (run-pipeline #f #f ("find" "." "-type" "f" "-name" "bystrotex.xml" "-exec" "dirname" "{}" ";") ("sort")))])
     (close-input-port inp)
     lns))
   
@(define (dirname x)
   (with-external-command-as
    dirname
    #:cmdline
    `("dirname" ,x)
    (read-line dirname-stdout)))

@(define (get-lookdown.html x)
   (let ([lookdown.html (build-path x "lookdown/lookdown.html")])
     (if (file-exists? lookdown.html)
         lookdown.html
         #f)))

@(define (strip-home-from-path x)
   (let ([cur (path->string x)]
         [hm  (path->string (find-system-path 'home-dir))])
     (if (string-prefix? cur hm) (substring cur (string-length hm)) cur)))

@title[#:style '(no-toc no-sidebar)]{@(strip-home-from-path (current-directory))}



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
         ,(table-of-contents)
         ,(tbl #:orient 'hor
               (for/list ([bystrotex-dir bystrotex.xml-dirs])
                 (list
                  (let ([lookdown.html (get-lookdown.html bystrotex-dir)])
                    (if lookdown.html (hyperlink lookdown.html bystrotex-dir) ""))
                  (seclink bystrotex-dir)))))
       '())
     ,@(for/list ([bystrotex-dir bystrotex.xml-dirs])
         (part 
          #f 
          (list (list 'part bystrotex-dir))
          `(,bystrotex-dir) 
          (make-style "LookdownPartFolder" '(unnumbered))
          '()
          (begin
                                        ;(displayln bystrotex.xml)
            (list (bystro-ribbon-for-location (string->path bystrotex-dir) #:exclude-same-name #t)))
          '())))))

@; ---------------------------------------------------------------------------------------------------
@(bystro-close-connection bystro-conf)
@disconnect[formula-database]
