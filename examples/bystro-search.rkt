#!/usr/bin/env racket

#lang racket

(require bystroTeX/xmlconf)

(define regular-expression
  (command-line
   #:program "search BystroTeX files in this folder for regexp"
   #:args (regular-expression) 
   regular-expression))

(call-with-input-file
  "lookdown.lst"
  (lambda (lookdown.lst)
    (for ([ln (cons "." (sequence->list (in-lines lookdown.lst)))])
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
                    (displayln (string-append  (path->string (path->complete-path (build-path ln name))) " :"))
                    (for ([page (directory-list (build-path ln name))] #:when (string-suffix? (path->string page) ".html"))
                      (when (call-with-input-file
                              (build-path ln name page)
                              (lambda (f) (for/or ([ln (in-lines f)]) (regexp-match (regexp regular-expression) ln))))
                        (displayln (string-append "  file://" (path->string (path->complete-path (build-path ln name page)))))))
                    )
                  (displayln (string-append "file://" (path->string (path->complete-path (build-path ln (dest . or . name)  name.html)))))))))))))
  
