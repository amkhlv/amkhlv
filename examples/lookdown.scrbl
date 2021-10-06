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
   (let* ([inp (run-pipeline #f #f ("find" "." "-mindepth" "2" "-type" "f" "-name" "bystrotex.xml") ("sort"))]
          [lns (port->lines (run-pipeline #f #f ("find" "." "-mindepth" "2" "-type" "f" "-name" "bystrotex.xml" "-exec" "dirname" "{}" ";") ("sort")))])
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

@(define (depth dir n)
   (let-values ([(base x y) (split-path dir)])
     (if (path? base)
         (if (link-exists? (build-path base "lookdown.scrbl"))
             (depth base (n . + . 1))
             (depth base n))
         n)))

@(define (strip-home-from-path x)
   (let ([cur (path->string x)]
         [hm  (path->string (find-system-path 'home-dir))])
     (if (string-prefix? cur hm) (substring cur (string-length hm)) cur)))

@title[#:style '(no-toc no-sidebar)]{@(strip-home-from-path (current-directory))}

@(bystro-ribbon)

@(table-of-contents)

@(decode
  (cons
   (title-decl 
    #f 
    '((part "LookdownMain"))
    #f
    (make-style "LookdownMainTitle" '(unnumbered toc-hidden))
    "")
   `(
     #|
     ,(part
       #f
       '((part "Lookdown"))
       '()
       (make-style "LookdownPart" '(unnumbered toc-hidden))
       '()
       `(
         ,(tbl #:orient 'hor
               (for/list ([bystrotex-dir bystrotex.xml-dirs])
                 (list
                  (let ([lookdown.html (get-lookdown.html bystrotex-dir)])
                    (if lookdown.html (hyperlink lookdown.html bystrotex-dir) ""))
                  (seclink bystrotex-dir)))))
       '()
       )
     |#
     ,@(for/list ([bystrotex-dir bystrotex.xml-dirs])
         (let ([lookdown.html (get-lookdown.html bystrotex-dir)])
           (part 
            #f 
            (list (list 'part bystrotex-dir))
            `(,bystrotex-dir) 
            (make-style
             "LookdownPartFolder"
             (if (and lookdown.html ((depth bystrotex-dir 0) . < . 2))
                 '(unnumbered)
                 '(unnumbered toc-hidden)))
            '()
            (begin
              (reverse
               (cons
                (bystro-ribbon-for-location (string->path bystrotex-dir) #:exclude-same-name #t)
                (if lookdown.html `(,(nested (hyperlink lookdown.html (path->string lookdown.html)))) '())
                )))
            '()))))))

@; ---------------------------------------------------------------------------------------------------
@(bystro-close-connection bystro-conf)
@disconnect[formula-database]
