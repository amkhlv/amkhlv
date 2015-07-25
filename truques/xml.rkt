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
(require scribble/core scribble/base scribble/html-properties scribble/decode)
(require racket/date)

(provide (all-from-out xml/path) (all-from-out racket/format))

(provide (contract-out [file->xexpr (-> path-string? the:xexpr?)]))
(define  (file->xexpr x)
  (call-with-input-file x
    (lambda (inport) (the:xml->xexpr (the:document-element (the:read-xml inport))))))

(define (unsanitize i)
  (cond [(char? i) (make-string 1 i)]
        [(or (eq? 10 i) (eq? 13 i)) "\n"]
        [(integer? i) (make-string 1 (integer->char i))] 
        [(string? i) (string-trim i)]))  

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

(provide (contract-out [show-xexpr (->* (the:xexpr?)  
                                        (#:transform-to-content
                                         (hash/c symbol? (-> the:xexpr? content?))
                                         #:transform-to-block
                                         (hash/c symbol? (-> the:xexpr? block?))
                                         #:size-step number?
                                         #:steps integer?
                                         ) 
                                        (or/c #f content? block?))]))
(define (show-xexpr 
         x 
         #:transform-to-content [t (make-hash '())] 
         #:transform-to-block   [tblock (make-hash '())]
         #:size-step [step 0.93] 
         #:steps [steps 4])
  (define (not-whitespace? u) (or (not (string? u)) (regexp-match #px"[^[:space:]]" u)))
  (define stl 
    (make-style 
     #f 
     (if 
      (> steps 0)
      (list (make-attributes 
             (list (cons 'style (format "font-size: ~a%;" (round (* 100 step)))))))
      '())))
  (define (show-tail xs)
    (let ([all-leafs? (for/and ([z xs]) (or (the:valid-char? z) (string? z)))]
          [all-content? (for/and ([z xs]) (or 
                                           (and (cons? z) (member (car z) (hash-keys t)))
                                           (content? z)))]
          [exist-leafs? (for/or ([z xs]) 
                          (and (or (the:valid-char? z) (string? z)) (not-whitespace? z)))])
      (if all-leafs?
          (verb 
           (apply 
            string-append
            (for/list ([i xs]) (unsanitize i))))
          ((if all-content? paragraph nested-flow)
           stl
           (for/list ([z xs] #:when (not-whitespace? z)) 
             (let ([a (show-xexpr z 
                                  #:transform-to-content t
                                  #:transform-to-block tblock
                                  #:size-step step
                                  #:steps (- steps 1))])
               (if (or all-content? (block? a)) a (para a))))))))
  (display "Processing: ") (displayln x)
  (cond
    [(symbol? x) (nested #:style stl (symbol->string x))]
    [(string? x) x]
    [(cons? x) 
     (define fn (let ([m (member (car x) (hash-keys t))]
                      [mblock (member (car x) (hash-keys tblock))]) 
                  (if m (hash-ref t (car m)) (if mblock (hash-ref tblock (car mblock)) #f))))
     (if fn
         (fn x)
         (cond 
          [(null? (cadr x)) ; (a () smth ...)
           (show-xexpr (cons (car x) (cddr x)) ; (a smth ...)
                       #:transform-to-content t
                       #:transform-to-block tblock
                       #:size-step step
                       #:steps steps)]
          [(the:xexpr? (cadr x)) ; (a XEXPR ...)
           (nested 
            #:style stl
            (tbl 
             (list 
              (list (symbol->string (car x)) ; a
                    (show-tail (cdr x))))))]
          [(and (list? (cadr x)) ; (a  ((b c) ...)  XEXPR  ...) 
                (for/and ([y (cadr x)]) (and (symbol? (car y)) (string? (cadr y)))))
           (nested
            #:style stl
            (tbl
             (list 
              (list 
               (tbl 
                (list (list (symbol->string (car x))) 
                      (list (tbl #:orient 'hor 
                                 (for/list ([y (cadr x)]) 
                                   (list (symbol->string (car y)) (cadr y)))))))
               (if (cons? (cddr x))
                   (show-tail (cddr x))
                   (nested #:style stl "---"))))))]))]
    ))
                              


(provide (contract-out [xsd:date->date* (-> string? date*?)]))
(define (xsd:date->date* x)
  (let* ([is-UTC? (char=? (last (string->list x)) #\Z)]
         [x+ (string-split x "+")]
         [positive-offset (and (cons? (cdr x+)) (cadr x+))]
         [xx (string-split x "-")]
         [y (string->number (car xx))]
         [m (string->number (cadr xx))]
         [d (string->number (substring (caddr xx) 0 2))])
    (define (hh:mm->sec t) 
      (let ([hm (string-split t ":")])
        (+ (* 3600 (string->number (car hm)))
           (* 60 (string->number (cadr hm))))))
    (define time-zone-offset 
      (cond
       [is-UTC? 0]
       [positive-offset (- (hh:mm->sec positive-offset))]
       [(cons? (cdddr xx)) (hh:mm->sec (cadddr xx))]
       [else #f]))
    (define dst-here? (date-dst? (current-date)))
    (define result
      (let ([summer? (and (not time-zone-offset) dst-here?)])
        (seconds->date
         (+ (or time-zone-offset 0)
            (date->seconds 
             (date 0 0 0 d m y 0 0 summer? 0)
             (not time-zone-offset)))
         (not time-zone-offset))))
    result
    ))
