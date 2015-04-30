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

  (require (planet jaymccarthy/sqlite))
  (require setup/dirs)
  (require racket/vector scribble/core scribble/base scribble/html-properties)
  (require (planet amkhlv/bystroTeX/common))

  (define myfilter-out-rows 
    (lambda (x #:rows-to-hide fields) 
      (let ([tst (lambda (field) (not (equal? (car x) field)))])
        (and (for/and ([f fields]) (tst f)) (not (equal? (car (cdr x)) "")))
	)
      )
    )
  (define mytransform-rows
    (lambda (x #:title-rows rows-to-highlight)
      (if (for/or ([row-name rows-to-highlight]) (equal? (car x) row-name))
          ;; (if (or (equal? (car x) "last") (equal? (car x) "first"))
          (list (car x) (elem #:style persname-style (car (cdr x))))
          (list (car x) (verb 
                         (let ([u (car (cdr x))])
                           (if u u "")) 
                         ))
          )
      )
    )
  (define cardtable-style
    (make-style "cardtable"
		(list (make-css-addition 
		       (build-path  (string->path "/home/andrei/usr/lib/racket/scribble/css/abkstyle.css"))
               )
              )
        )
    )

  (define persname-style
    (make-style "persname"
		(list (make-css-addition 
		       (build-path (string->path "/home/andrei/usr/lib/racket/scribble/css/abkstyle.css")
				   )
		       )
		      )
		)
    )

  (define mytranspose-vecs
    (lambda (x)
      (let ([n (vector-length (car x))])
        (build-list n 
		    (lambda (j) 
		      (let ([g (lambda (vec) (vector-ref vec j))]) (map g x))
		      )
		    )
	)
      )
    )
  
  (define sqli-rows 
    (lambda (dbfilename query ttls rsth)
      (let* (
             [mydb (open (string->path dbfilename))]
             [myres  (select mydb query)]
             [colnames (if (pair? myres) (car myres) #f)]
             [rows (if (pair? myres) (cdr myres) #f)]
             [printrow
              (if (pair? myres)
              (lambda (row)
                (tabular 
                 (map (lambda (x) (mytransform-rows x #:title-rows ttls))
                      (filter (lambda (r) (myfilter-out-rows r #:rows-to-hide rsth))  
                              (mytranspose-vecs (list colnames row)))
                      ) 
                 #:style cardtable-style
                 ))
              #f)])
        (close mydb)
        (if (pair? myres)
            (map printrow rows)
            '("*** No matches in the database ***"))
        )))

  (provide mysqli-tables)
  (define mysqli-tables
    (lambda (#:dbfile dbf #:sql query #:to-highlight [ttls '()] #:to-hide [rsth '()])
      (sqli-rows dbf query ttls rsth)
      )
    )
  )
