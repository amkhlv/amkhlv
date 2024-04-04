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


(require racket/base xml/xexpr)

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
(define (prepattr attrs) (filter cadr attrs))
(provide (contract-out [p (->*
                           (any/c)
                           (#:size (or/c string? #f)
                            #:color (or/c string? #f)
                            #:align (or/c string? #f)
                            )
                           xexpr/c)]))
(define (p
         #:size [sz #f] #:color [clr #f] #:align [aln #f]
         expr)
  (append
   `(p ,(prepattr `([size ,sz] [color ,clr] [align ,aln])))
   (let rec ([attrs (prepattr `([size ,sz] [color ,clr] [align ,aln]))] [x expr])
     (match x
       [str
        #:when (string? str)
        `((r ,attrs ,str))]
       [(a-link h t)
        `((a ([href ,h]) ,t))]
       [(a-image s c)
        `((a ([src ,s]) ,c))]
       [(a-bold xs)
        (append-map (λ (e) (rec (ins '(b "×") attrs) e)) xs)]
       [(a-italic xs)
        (append-map (λ (e) (rec `((i "×") ,@attrs) e)) xs)]
       [(a-underline xs)
        (append-map (λ (e) (rec `((u "×") ,@attrs) e)) xs)]
       )
     )
   )
  )


(define (docx-xexpr expr)
  (apply
   (cons 'root 
         (let rec ([attrs '()] [x expr])
           (match x
             [str
              #:when (string? str)
              `((r ,attrs ,str))]
             [(a-bold xs)
              (append-map (λ (e) (rec (ins '(b "") attrs) e)) xs)]
             [(a-italic xs)
              (append-map (λ (e) (rec `((i "") ,@attrs) e)) xs)]
             [(a-underline xs)
              (append-map (λ (e) (rec `((u "") ,@attrs) e)) xs)]
             )
           )
         )
   )
  )




