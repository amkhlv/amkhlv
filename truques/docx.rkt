#lang racket

#|
Copyright 2024 Andrei Mikhailov

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


(require racket/base xml)

(struct a-bold (contents))
(provide b)
(define (b . x) (a-bold x))
(struct a-italic (contents))
(provide i)
(define (i . x) (a-italic x))
(struct a-underline (contents))
(provide u)
(define (u . x) (a-underline x))
(struct a-link (href text))
(provide a)
(define (a h t) (a-link h t))
(struct a-image (src caption))
(provide img)
(define (img s c) (a-image s c))
(define (ins a as)
  (if (member (car a) (map car as)) as (cons a as)))
(define (prepattr attrs)
  (let ([x (filter cadr attrs)])
    (if (empty? x) '() `(,x))))
(provide (contract-out [p (->*
                           ()
                           (#:size (or/c string? #f)
                            #:color (or/c string? #f)
                            #:align (or/c string? #f)
                            )
                           #:rest any/c
                           xexpr/c)]))
(define (p #:size [sz #f] #:color [clr #f] #:align [aln #f]
           . exprs)
  (append
   `(p ,@(prepattr `([size ,sz] [color ,clr] [align ,aln])))
   (append-map
    (λ(expr)
      (let rec ([attrs '()] [x expr])
        (match x
          [str
           #:when (string? str)
           `((r ,@(prepattr attrs) ,str))]
          [(a-link h t)
           `((a ([href ,h]) ,t))]
          [(a-image s c)
           `((img ([src ,s]) ,c))]
          [(a-bold xs)
           (append-map (λ (e) (rec (ins '(b "") attrs) e)) xs)]
          [(a-italic xs)
           (append-map (λ (e) (rec (ins '(i "") attrs) e)) xs)]
          [(a-underline xs)
           (append-map (λ (e) (rec (ins '(u "") attrs) e)) xs)]
          )
        )
      )
    exprs)
   )
  )
(provide (contract-out [t (->* () () #:rest (listof xexpr/c) xexpr/c)]))
(define (t . rows) `(table ,@rows))
(provide (contract-out [tr (->* () () #:rest (listof xexpr/c) xexpr/c)]))
(define (tr . cells) `(tr ,@cells))
(provide (contract-out [td (->* () () #:rest (listof xexpr/c) xexpr/c)]))
(define (td . contents) `(td ,@contents))

(provide (contract-out [docx-xexpr (->* () () #:rest (listof xexpr/c) xexpr/c)]) )
(define (docx-xexpr . expr)
  `(root ,@expr))

(provide (contract-out [docx->file (->* (path-string? xexpr/c) () void?)]))
(define (docx->file file-path doc-xexpr)
    (let-values
      ([(proc out in err)
        (subprocess #f #f #f (find-executable-path "xml2docx") "-o" file-path)])
      (display (xexpr->string doc-xexpr) in)
      (close-output-port in)
      (display (port->string err) (current-error-port))
      (close-input-port err)
      (close-input-port out)
    ))
  




