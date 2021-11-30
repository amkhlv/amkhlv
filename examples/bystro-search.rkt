#!/usr/bin/env racket

#lang racket

(require bystroTeX/xmlconf)

(define find-lookdowns (make-parameter #f))
(define search-here (make-parameter #f))

(define regular-expression
  (command-line
   #:program "search BystroTeX files in this folder for regexp"
   #:once-each
   [("-l" "--search-for-lookdowns") "search for lookdowns" (find-lookdowns #t)]
   [("-t" "--search-this-bystro-dir") "search this bystro dir (does not have to be lookdown)" (search-here #t)]
   #:args ([regular-expression 'ERROR:missing-regexp])
   regular-expression))

(define (search-in-bystro-dir ln)
  (parameterize ([working-directory ln])
    (for ([nm (all-names)])
      (with-bystroconf
        (get-bystroconf nm)
        (name dest name.html name.scrbl formulas/ .sqlite arglist multipage?)
        ;(displayln (list name dest name.html name.scrbl formulas/ .sqlite arglist multipage?))
        (when (call-with-input-file
                (build-path ln name.scrbl)
                (lambda (f) (for/or ([ln (in-lines f)]) (regexp-match (regexp regular-expression) ln))))
          (if multipage?
              (begin
                (displayln (string-append  "-------- " (path->string (path->complete-path (build-path ln name))) " :"))
                (with-handlers
                  ([exn:fail?
                    (lambda (exn)
                      (displayln "--------------------------- ERROR ----------------------------------------------------------------------")
                      (displayln exn)
                      (displayln "--------------------------- ERROR ----------------------------------------------------------------------")
                      )])
                  (displayln (string-append "  file://" (path->string (path->complete-path (build-path ln name "index.html")))))
                  (for ([page (directory-list (build-path ln name))] #:when (string-suffix? (path->string page) ".html"))
                    (when (call-with-input-file
                            (build-path ln name page)
                            (lambda (f) (for/or ([ln (in-lines f)]) (regexp-match (regexp regular-expression) ln))))
                      (displayln (string-append "    file://" (path->string (path->complete-path (build-path ln name page))))))))
                )
              (displayln (string-append "file://" (path->string (path->complete-path (build-path ln (dest . or . name)  name.html)))))))))))

(if (search-here)
    (search-in-bystro-dir ".")
    (call-with-input-file
      "lookdown.lst"
      (lambda (lookdown.lst)
        (for ([ln (cons "." (sequence->list (in-lines lookdown.lst)))])
          (if (find-lookdowns)
              (when ((link-exists? (build-path ln "lookdown.scrbl")) . or . (file-exists? (build-path ln "lookdown.lst")))
                (displayln ln))
              (search-in-bystro-dir ln)
              )))))
  
