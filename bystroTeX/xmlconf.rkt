#|
Copyright 2012,2013 Andrei Mikhailov

This file is part of bystroTeX.

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

(module xmlconf racket
  (require xml xml/path racket/string (for-syntax racket/syntax))
  
  (provide (contract-out [working-directory parameter?]))
  (define working-directory (make-parameter #f))
  ;; some default values
  (define default-sqlite-filename-in-dest-folder   "formulas.sqlite")
  (define default-sqlite-filename-in-curdir_suffix "_formulas.sqlite")

  (provide (contract-out [xml-file->bystroconf-xexpr (-> path-string? xexpr?)]))
  (define (xml-file->bystroconf-xexpr xf)
    (call-with-input-file xf (λ (inport) (xml->xexpr (document-element (read-xml inport))))))

  (provide (contract-out [bystroconf-xexpr (-> (or/c xexpr? #f))]))
  (define (bystroconf-xexpr)
    (let ([f (build-path (or (working-directory) 'same) "bystrotex.xml")])
      (if (file-exists? f)
          (xml-file->bystroconf-xexpr f)
          #f)))

  (provide (contract-out [all-names (-> (listof string?))]))
  (define (all-names)
    (for/list ([c (se-path*/list '(scribblings) (or (bystroconf-xexpr) 'empty))] #:when (cons? c))
      (string-trim (se-path* '(name) c))))

  (provide (contract-out
            [get-bystroconf (-> string? (or/c xexpr? #f))]))
  (define (get-bystroconf x)
    (for/first ([y (se-path*/list '(scribblings) (or (bystroconf-xexpr) 'empty))]
                #:when
                (and 
                 (cons? y)
                 (equal? (string-trim (se-path* '(name) y)) x)))
      y))
        
  (provide with-bystroconf)
  (define-syntax (with-bystroconf stx)
    (syntax-case stx ()
      [(_ c (Xname Xdest Xname.html Xname.scrbl Xformulas/ X.sqlite Xarglist Xmultipage?) body ...)
       #'(let* ([Xname (let ([v (se-path* '(name) c)]) (if v (string-trim v) #f))]
                [Xdest (let ([v (se-path* '(dest) c)]) (if v (string-trim v) #f))]
                [formulas-dir (let ([v (se-path* '(formulas-dir) c)]) (if v (string-trim v) #f))]
                [sqlite-file  (let ([v (se-path* '(sqlite-file) c)]) (if v (string-trim v) #f))]
                [Xname.html (if Xname (string->path (string-append Xname ".html")) #f)]
                [Xname.scrbl (if Xname (string->path (string-append Xname ".scrbl")) #f)]
                [Xformulas/ (or formulas-dir Xname)]
                [Xarglist (flatten 
                             (for/list ([a (se-path*/list '(args) c)] #:when (cons? a))
                               (if (cons? (se-path*/list '(value) a))
                                   (list "++arg" 
                                         (string-append "--" (string-trim (se-path* '(value #:key) a))) 
                                         "++arg" 
                                         (string-trim (se-path* '(value) a)))
                                   (list "++arg" (string-append "--" (string-trim (se-path* '(flag) a)))))))]
                [Xmultipage? (cons?    (filter   (λ  (x)  (equal? x '(multipage ())))   c))]
                [X.sqlite  
                 (or sqlite-file 
                     (if Xdest
                         (path->string (build-path Xdest default-sqlite-filename-in-dest-folder))
                         (if Xname
                             (if Xmultipage? 
                                 (path->string (build-path Xname default-sqlite-filename-in-dest-folder))
                                 (string-append Xname default-sqlite-filename-in-curdir_suffix))
                             #f)))])
           body ...)]))

  )
