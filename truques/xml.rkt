#lang racket

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


(require (prefix-in the: xml) xml/path racket/format)
(require (planet amkhlv/bystroTeX/common))
(require scribble/core scribble/base scribble/decode)

(provide (all-from-out xml/path) (all-from-out racket/format))

(provide (contract-out [file->xexpr (-> path-string? the:xexpr?)]))
(define  (file->xexpr x)
  (call-with-input-file x
    (lambda (inport) (the:xml->xexpr (the:document-element (the:read-xml inport))))))

(provide (contract-out [xexprs->nested-flow (->* ((listof the:xexpr?)) (#:style any/c) (or/c #f nested-flow?))]))
(define (xexprs->nested-flow xs #:style [s #f])
  (if (cons? xs) 
      (keyword-apply 
       nested
       '(#:style)
       (list s)
       (map (λ (y) 
              (cond [(char? y) (make-string 1 y)]
                    [(or (eq? 10 y) (eq? 13 y)) (linebreak)]
                    [(integer? y) (make-string 1 (integer->char y))] 
                    [else y])) 
            xs))
      #f))

         
