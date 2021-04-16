#|
Copyright 2012,2013 Andrei Mikhailov

This file is part of truques.

bystroTeX is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

bystroTeX is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with bystroTeX.  If not, see <http://www.gnu.org/licenses/>.
|#

(module truques racket
  (require scribble/core scribble/base scribble/html-properties scribble/decode scriblib/render-cond racket/string racket/path)
  (require bystroTeX/common)
  (require xml/path (prefix-in the: xml))
  
  (define copy-tag-num 0)

  (provide (contract-out [show-and-go (->* (namespace-anchor?) () #:rest (listof string?) block?)]))
  (define (show-and-go a . x)
    (define thisns (namespace-anchor->namespace a))
    (let ((mycode (apply string-append x)))
        (nested (nested #:style (style "comment" '()) (verb mycode))
                (eval (read (open-input-string (string-append "(begin " mycode ")"))) thisns))))

  (provide (contract-out [curdir (-> element?)]))
  (define (curdir)
                                        ;  Inserts the link to the current dir
    (hyperlink (bystro-path-to-link ".") 
               #:style (make-style 
                        "sourcelink" 
                        (list (make-css-addition "misc.css"))) 
               "*dir*"))
  (provide (contract-out [mailto (->* () () #:rest (listof string?) element?)]))
  (define (mailto . x)
    (let* ([xx (map 
                (λ (u) (string-trim u #px"\\<|\\>")) 
                (filter 
                 ((curry regexp-match?) #rx"@") 
                 (apply append (map string-split x))))]
           [z (apply 
               ((curry string-append) "mailto:")
               (add-between xx ","))])
      (hyperlink z (add-between (filter (compose not ((curry regexp-match?) #px"^\\s*$")) x) " ⋄ "))))

  (provide (contract-out [copy-to-clipboard (->* () (#:rows (or/c integer? #f) #:cols (or/c integer? #f)) #:rest (listof string?) element?)]))
  (define (copy-to-clipboard #:rows [rows #f] #:cols [cols #f] . xs)
    (set! copy-tag-num (+ 1 copy-tag-num))
    (element
     (style #f '())
     (list
      (make-element
       (make-style
        "bystro-copy-to-clipboard"
        `(,(alt-tag "textarea")
          ,(attributes
            `(,(cons 'id (string-append "amkhlv-bystro-copy-id-" (number->string copy-tag-num)))
              ,(cons 'readonly "1")
              ,@(filter (lambda (x) (cdr x))
                        `(,(cons 'rows (if rows (number->string rows) #f))
                          ,(cons 'cols (if cols (number->string cols) #f))))))))
       (apply string-append xs))
      (tg
       button
       #:attrs ([onclick (string-append "amkhlvBystroCopyFn" (number->string copy-tag-num) "()")])
       "COPY")
      (tg
       script
       (string-append
        "function amkhlvBystroCopyFn"
        (number->string copy-tag-num)
        "() {var copyText = document.getElementById(\""
        "amkhlv-bystro-copy-id-"
        (number->string copy-tag-num)
        "\") ; copyText.select(); document.execCommand(\"copy\"); }"
        )
       )
      )
     )
    )
  (provide (contract-out [autolist (->*
                                    ()
                                    (#:exts (listof symbol?)
                                     #:dir path-string?
                                     #:header (or/c (listof any/c) #f)
                                     #:output (-> path-string? (or/c (listof any/c))))
                                    (or/c table? element?))]))
  (define (autolist
           #:exts [extensions '(pdf)]
           #:dir [dir (get-bystro-scrbl-name)]
           #:header [header #f]
           #:output [o
                     (lambda (f)
                       `(,(hyperlink
                           (find-relative-path
                            (current-directory)
                            (path->complete-path (build-path dir f)))
                           (path->string f))))]
           )
    (displayln "")
    (displayln dir)
    (let ([relevant-files
           (for/list
               ([f (directory-list dir)]
                #:when (for/or ([ext (map symbol->string extensions)])
                         (string-suffix? (path->string f) (string-append "." ext))))
             (o f))])
      (if (cons? relevant-files)
          (bystro-table
           #:style-name "bystro-autolist"
           (if (cons? header) (cons header relevant-files) relevant-files))
          (make-element
           (make-style "bystro-autolist-nothing-found" '())
           `("no files with extensions: "
             ,(string-join (map symbol->string extensions) "|"))))))
  (provide (contract-out [check (->* () () #:rest (listof any/c) element?)]))
  (define (check . xs)
    (make-element
     (make-style #f (list (alt-tag "label") (attributes `(,(cons 'class "bystro-checkbox-label")))))
     (append
      xs
      `(
        ,(make-element
          (make-style #f (list (alt-tag "input")
                               (attributes `(
                                             ,(cons 'type "checkbox")
                                             ,(cons 'class "bystro-checkbox")))))
          '())
        ,(make-element
          (make-style #f (list (alt-tag "span") (attributes `(,(cons 'class "bystro-checkmark")))))
          '()))
      )))

  (provide (contract-out [autolist-pdfs (->*
                                         ()
                                         (#:dir path-string?)
                                         (or/c table? element?))]))
  (define (autolist-pdfs #:dir [dir 'same])
    (autolist
     #:exts '(pdf PDF)
     #:dir dir
     #:header `(,(bold "summary") ,(bold "PDF"))
     #:output (lambda (f)
                (let* ([frel
                        (find-relative-path
                         (current-directory)
                         (path->complete-path (build-path dir f)))]
                       [.pdf (path->string f)]
                       [.pdq (path-replace-extension frel ".pdq")]
                       [x (if
                           (file-exists? .pdq)
                           (call-with-input-file .pdq
                             (lambda (inport) (the:xml->xexpr (the:document-element (the:read-xml inport)))))
                           '(root () (summary () "--")))]
                       [summary (se-path* '(summary) x)])
                  `(
                    ,(or summary "")
                    ,(hyperlink frel .pdf))
                  )
                )
     )
    )
  (provide (contract-out [autolist-images (->*
                                           ()
                                           (#:exts (listof symbol?)
                                            #:dir path-string?
                                            #:scale number?
                                            #:ncols integer?)
                                           (or/c table? element?))]))  
  (define (autolist-images
           #:exts [extensions '(svg png tiff jpg jpeg)]
           #:dir [dir 'same]
           #:scale [scale 0.25]
           #:ncols [ncols 2])
    (define (complement-list lst n)
      (if (equal? (length lst) n)
          lst
          (complement-list (cons "" lst) n)))
    (define/match (split-list-in-pairs lst acc)
      [('() (cons row aa)) (reverse (map reverse (cons (complement-list row ncols) aa)))]
      [((cons el rst) (cons row aa))
       #:when (equal? (length row) ncols)
       (split-list-in-pairs rst (cons (list el) (cons row aa)))]
      [((cons el rst) (cons row aa))
       (split-list-in-pairs rst (cons (cons el row) aa))]
      [((cons el rst) '())
       (split-list-in-pairs rst (list (list el)))])
    (let ([relevant-files
           (for/list
               ([f (directory-list dir)]
                #:when (for/or ([ext (map symbol->string extensions)])
                         (string-suffix? (path->string f) (string-append "." ext))))
             (hyperlink (build-path dir f) (image #:scale scale (build-path dir f))))])
      (if (cons? relevant-files)
          (tbl (split-list-in-pairs relevant-files '()))
          (make-element
           (make-style "bystro-autolist-nothing-found" '())
           `("no files with extensions: "
             ,(string-join (map symbol->string extensions) "|")))
          )
      )
    )
  (provide (contract-out [autolist-svgs (->*
                                         ()
                                         (#:dir path-string?
                                          #:scale number?
                                          #:ncols integer?)
                                         (or/c table? element?))]))
  (define autolist-svgs (curry autolist-images #:exts '(svg)))


  )
