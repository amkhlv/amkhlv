#|
Copyright 2012-2015 Andrei Mikhailov

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


(module sqlite racket
  
  (require db/base db/sqlite3)
  (require racket/list racket/vector scribble/core scribble/base scribble/html-properties racket/string)
  (require bystroTeX/common)

  (provide (all-from-out db/base) (all-from-out db/sqlite3))

  (provide (contract-out
                                        ; get column titles
            [get-column-names (-> connection? #:table string? (listof string?))]))
  (define (get-column-names conn #:table t)
    (let* ([row-vectors (query-rows conn (string-append "pragma table_info(" t ")"))])
      (for/list ([v row-vectors]) (vector-ref v 1))))
                                

  (provide (contract-out 
                                        ; print sqlite tables
            [mysqli-tables (->*
                            (connection? 
                             #:sql 
                             string?
                             #:column-titles 
                             (listof string?)
                             #:css
                             path-string?)
                            (#:params
                             (listof any/c)
                             #:to-highlight
                             (listof string?)
                             #:to-hide
                             (listof string?))
                            (listof block?))]))
  (provide (contract-out
            ; sanitize dollar sign
            [$->_ (-> string? string?)]))



  (define (mysqli-tables
           conn
           #:column-titles column-titles
           #:sql x
           #:css css-filename
           #:params [prms '()] 
           #:to-highlight [rows-to-hl '()] 
           #:to-hide [rows-to-hide '()])
    (let* ([lookup (prepare conn x)]
           [row-vectors (query-rows conn (bind-prepared-statement lookup prms))])
      (for/list ([row-vector row-vectors])
        (let* ([all-table-rows 
                (map
                 (λ (j) 
                   `(,(list-ref column-titles j) 
                     ,(vector-ref row-vector j)))
                 (range (length column-titles)))]
               [rows-to-show 
                (filter (λ (x) 
                          (and (not (member (car x) rows-to-hide))
                               (and (cadr x) (not (equal? (cadr x) "")))))
                          all-table-rows)]
               [formatted-table-rows
                (map
                 (λ (x) 
                   `(,(car x) 
                     ,(let* ([xc (cadr x)]
                             [y (if (sql-null? xc) "" xc)])
                        (if (not (member (car x) rows-to-hl)) (verb y) (bold y)))))
                 rows-to-show)])
          (tabular 
           #:style 
           (make-style "cardtable"
                       (list (make-css-addition 
                              (build-path  (string->path css-filename)))))
           formatted-table-rows)))))
  (define ($->_ x) (string-replace x "$" "_"))
  )
