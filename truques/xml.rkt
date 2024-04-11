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


(require (prefix-in the: xml) xml/path xml/xexpr racket/format)
(require bystroTeX/common)
(require scribble/core scribble/base scribble/html-properties scribble/decode)
(require scribble/srcdoc (for-doc scribble/base scribble/manual))
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

(provide (contract-out [show-xexpr-size (parameter/c (or/c integer? #f))]))
(provide (contract-out [show-xexpr-size-step (parameter/c number?)]))
(provide (contract-out [show-xexpr-steps (parameter/c integer?)]))
(define show-xexpr-size (make-parameter #f))
(define show-xexpr-size-step (make-parameter 0.93))
(define show-xexpr-steps (make-parameter 4))

(define (recurse-element html-tag)
  (λ (x)
    (element
        (make-style (se-path* `(,html-tag #:class) x) (list (alt-tag (symbol->string html-tag))))
      (for/list ([a (cddr x)]) (if (the:xexpr? a) (show-xexpr a) (element a))))))

(provide (contract-out [transform-to-content (parameter/c (hash/c symbol? (-> the:xexpr? content?) #:immutable #t))]))
(provide (contract-out [transform-to-block (parameter/c (hash/c symbol? (-> the:xexpr? block?) #:immutable #t))]))
(define transform-to-content 
  (make-parameter 
   (hash 'a (λ (x) (hyperlink (string-trim (se-path* '(a #:href) x)) (se-path* '(a) x)))
         'b (λ (x) (bold (se-path* '(b) x)))
         'it (λ (x) (italic (se-path* '(it) x)))
         'nbsp (λ (x) ~)
         'emsp (λ (x) (hspace 1))
         'br (λ (x) (linebreak))
         'span (recurse-element 'span)
         'div  (recurse-element 'div)
         'p    (recurse-element 'p)
         )))
(define transform-to-block
  (make-parameter
   (hash 'verbatim (λ (x) (verb (se-path* '(verbatim) x)))
         )))

(provide (contract-out [show-xexpr (->* (the:xexpr?)  
                                        (#:transform-to-content
                                         (hash/c symbol? (-> the:xexpr? content?))
                                         #:transform-to-block
                                         (hash/c symbol? (-> the:xexpr? block?))
                                         #:show-root boolean?
                                         #:size (or/c integer? #f)
                                         #:size-step number?
                                         #:steps integer?
                                         ) 
                                        (or/c #f content? block?))]))
(define (show-xexpr 
         x 
         #:transform-to-content [t (transform-to-content)] 
         #:transform-to-block   [tblock (transform-to-block)]
         #:show-root [sr #f]
         #:size [size (show-xexpr-size)]
         #:size-step [step (show-xexpr-size-step)] 
         #:steps [steps (show-xexpr-steps)])
  (let ([result (show-xexpr0 
                 x 
                 #:transform-to-content t
                 #:transform-to-block   tblock
                 #:show-root sr 
                 #:size-step step
                 #:steps steps)])
    (if size 
        (nested-flow 
         (make-style #f (list (make-attributes (list (cons 'style (format "font-size: ~a%;" size))))))
         (list (if (block? result) result (para result))))
        result)))

(define (show-xexpr0
         x 
         #:transform-to-content [t (transform-to-content)] 
         #:transform-to-block   [tblock (transform-to-block)]
         #:show-root [sr #f]
         #:size-step [step (show-xexpr-size-step)]
         #:steps [steps (show-xexpr-steps)])
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
             (let ([a (show-xexpr0 z 
                                   #:transform-to-content t
                                   #:transform-to-block tblock
                                   #:size-step step
                                   #:show-root #t
                                   #:steps (- steps 1))])
               (if (or all-content? (block? a)) a (para a))))))))
  ;;(display "Processing: ") (displayln x)
  (cond
    [(symbol? x) (nested #:style stl (symbol->string x))]
    [(string? x) x]
    [(cons? x) 
     (define fn (let ([m (member (car x) (hash-keys t))]
                      [mblock (member (car x) (hash-keys tblock))]) 
                  (if m (hash-ref t (car m)) (if mblock (hash-ref tblock (car mblock)) #f))))
     (if fn
         (parameterize ([show-xexpr-size-step step] [show-xexpr-steps steps]) (fn x))
         (cond 
          [(null? (cdr x)) ; (a)
           (tbl (list (list (symbol->string (car x)))))]
          [(null? (cadr x)) ; (a () smth ...)
           (show-xexpr0 (cons (car x) (cddr x)) ; (a smth ...)
                        #:transform-to-content t
                        #:transform-to-block tblock
                        #:show-root sr
                        #:size-step step
                        #:steps steps)]
          [(the:xexpr? (cadr x)) ; (a XEXPR ...)
           (nested 
            #:style stl
            (if sr
                (tbl 
                 (list 
                  (list (symbol->string (car x)) ; a
                        (show-tail (cdr x)))))
                (show-tail (cdr x))))]
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

; DOCX
(define (docx-make-style-props attrs)
  (let* ([html-attrs
          (let rec ([xs attrs])
            (if (empty? xs)
                ""
                (string-append
                 (match (car xs)
                   [(list 'b _) "font-weight:bold;"]
                   [(list 'i _) "font-style:italic;"]
                   [(list 'size n) (format "font-size:~apt;" n)]
                   [(list 'align a) (format "text-align:~a;" a)]
                   [_ ""])
                 (rec (cdr xs))
                 )
                )
            )]
         [hattrs (if (empty? html-attrs) '() `(,(attributes (list (cons 'style html-attrs)))))]
         )
    (let rec ([xs attrs])
      (if (empty? xs)
          hattrs
          (match (car xs)
            [(list 'color rgb)
             (let ([r (string->number (substring rgb 0 2) 16)]
                   [g (string->number (substring rgb 2 4) 16)]
                   [b (string->number (substring rgb 4 6) 16)])
               (cons (color-property (list r g b)) (rec (cdr xs))))]
            [_ (rec (cdr xs))])))))
(define (show-run r)
  (let-values
      ([(attrs text)
        (if ((cons? (car r)) . and . (cons? (car (car r)))) ; first element is list of attributes
            (values (car r) (cdr r))
            (values '() r))])
    (element
     (style "docx-run" (docx-make-style-props attrs))
     `(,text))))
(define (show-link a)
  (let-values
      ([(attrs text)
        (values (car a) (cadr a))])
    (hyperlink
     (cadr (findf (λ(x) (eq? 'href (car x))) attrs))
     text)))
(define (show-image img)
  (let-values
      ([(src caption)
        (values (car (car img)) (cadr img))])
    (image src caption)))
(define (show-docx-paragraph p)
  (let-values
      ([(attrs contents)
        (if ((cons? (car p)) . and . (cons? (car (car p)))) ; first element is list of attributes
            (values (car p) (cdr p))
            (values '() p))])
    (paragraph
     (style "docx-paragraph" (docx-make-style-props attrs))
     (for/list ([e contents])
       (match (car e)
         ['r (show-run (cdr e))]
         ['a (show-link (cdr e))]
         ['img (show-image (cdr e))])))))
(define (show-docx-table rows)
  (tabular
   (map
    (λ(row)
      (map
       (λ(cell)
         (apply
          nested
          (for/list ([e (cdr cell)])
            (match (car e)
              ['p (show-docx-paragraph (cdr e))]
              ['table (show-docx-table (cdr e))]
              ))
          #:style (style "docx-table-cell" '())
          )
         )
       (cdr row))
      )
    rows)
   )
  )
(provide (proc-doc show-docx (->i ([doc xexpr/c]) () [result (or/c #f content? block?)]) ("show docx")))
(define (show-docx doc)
  (apply
   nested
   (for/list ([e (cdr doc)])
     (match (car e)
       ['p (show-docx-paragraph (cdr e))]
       ['table (show-docx-table (cdr e))]
       )
     )
   #:style (style "docx" '())
   )
  )
   
   
   
