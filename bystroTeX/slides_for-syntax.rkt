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

(module slides_for-syntax racket

  (provide bystro-formula-syntax)
  (define (bystro-formula-syntax #:autoalign-formula-prefix auto-prefix 
                                 #:manual-formula-prefix    formula-prefix 
                                 #:display-math-prefix      display-math-prefix 
                                 #:size-change-notation     size-change-id 
                                 #:size-increase-notation   size-increase-id
                                 #:size-restore-notation    size-restore-id 
                                 #:max-vert-adjust          [max-vert-adjust 12]
                                 #:max-size-increase        [max-size-increase 9]
                                 #:max-size-decrease        [max-size-decrease 9]
                                 #:calc-scale
                                 [calc-scale
                                  (lambda (i)
                                    (let rec ([j i])
                                      (cond
                                        [(zero? j) 1]
                                        [(positive? j) (* 1.1 (rec (j . - . 1)))]
                                        [(negative? j) (* 0.9 (rec (j . + . 1)))])))]
                                 #:color-reset-command [color-reset-command "bystro-reset-colors"]
                                 stx)
    (let* (
           [formula-db `(define formula-database
                          #|(begin 
                          (configure-bystroTeX-using bystro-conf)|#
                          (bystro-initialize-formula-collection)
                          )]
           [auto `(define ,(string->symbol auto-prefix) 
                    (lambda u (bystro-formula (apply string-append u))))]
           [disp `(define ( ,(string->symbol display-math-prefix) 
                            #:label (lbl #f) 
                            #:scale (n 1)
                            #:css-class (css-class #f)
                            . x)
                    (if css-class
                        (bystro-equation #:label lbl #:scale n #:css-class css-class  x)
                        (bystro-equation #:label lbl #:scale n  x)
                        ))]
           [ch-sz `(define (,(string->symbol size-change-id) (i #f) (aaadj #f)) (displayln "deprecated size-change"))]
           [inc-sz `(define (,(string->symbol size-increase-id) i (aaadj #f)) (displayln "deprecated size-change"))]
           [rs-sz  `(define (,(string->symbol size-restore-id)) (displayln "deprecated size-reset"))]
           [rs-clr `(define (,(string->symbol color-reset-command)) (displayln "color-reset is deprecated"))]
           [ttp-init '(bystro-titlepage-init #:singlepage-mode singlepage-mode)]
           [l+ (lambda (m)
                 `(define 
                    (,(string->symbol (format "~a+~a" formula-prefix m)) . u)
                    (bystro-formula #:align ,m
                                    (apply string-append u))))]
           [l- (lambda (m)
                 `(define 
                    (,(string->symbol (format "~a-~a" formula-prefix m)) . u)
                    (bystro-formula #:align ,(-  m)
                                    (apply string-append u))))]
           [l++ (lambda (m z)
                  `(define 
                     (,(string->symbol (format "~a+~a+~a" formula-prefix m z)) . u)
                     (bystro-formula #:align ,m
                                     #:scale  ,(calc-scale z)
                                     (apply string-append u))))]
           [l+- (lambda (m z)
                  `(define 
                     (,(string->symbol (format "~a+~a-~a" formula-prefix m z)) . u)
                     (bystro-formula #:align ,m
                                     #:scale  ,(calc-scale (- z))
                                     (apply string-append u))))]
           [l-+ (lambda (m z)
                  `(define 
                     (,(string->symbol (format "~a-~a+~a" formula-prefix m z)) . u)
                     (bystro-formula #:align ,(- m)
                                     #:scale ,(calc-scale z)
                                     (apply string-append u))))]
           [l-- (lambda (m z)
                  `(define 
                     (,(string->symbol (format "~a-~a-~a" formula-prefix m z)) . u)
                     (bystro-formula #:align ,(- m)
                                     #:scale ,(calc-scale (- z)) 
                                     (apply string-append u))))]
           [set-css-dir '(define (bystro-set-css-dir x) 
                           (bystro-set-css-dir_common x) 
                           (bystro-set-css-dir_slides x))]
           [def-list+ (for/list ([i (build-list (+ 1 max-vert-adjust) values)]) (l+ i))]
           [def-list- (for/list ([i (build-list (+ 1 max-vert-adjust) values)]) (l- i))]
           [def-list++ (apply append (for/list ([i (build-list (+ 1 max-vert-adjust) values)]) 
                                       (for/list ([z (build-list (+ 1 max-size-increase) values)]) 
                                         (l++ i z))))]
           [def-list+- (apply append (for/list ([i (build-list (+ 1 max-vert-adjust) values)]) 
                                       (for/list ([z (build-list (+ 1 max-size-decrease) values)]) 
                                         (l+- i z))))]
           [def-list-+ (apply append (for/list ([i (build-list (+ 1 max-vert-adjust) values)]) 
                                       (for/list ([z (build-list (+ 1 max-size-increase) values)]) 
                                         (l-+ i z))))]
           [def-list-- (apply append (for/list ([i (build-list (+ 1 max-vert-adjust) values)]) 
                                       (for/list ([z (build-list (+ 1 max-size-decrease) values)]) 
                                         (l-- i z))))]
           )
      (syntax-case stx () 
        [(_ x) (datum->syntax 
                #'x
                (append (list 
                         'begin 
                         formula-db
                         auto 
                         disp 
                         ch-sz 
                         inc-sz 
                         rs-sz
                         rs-clr
                         ttp-init
                         set-css-dir
                         ) 
                        def-list+ 
                        def-list-
                        def-list++ 
                        def-list+- 
                        def-list-+ 
                        def-list--
                        )
                #'x #'x #'x)])
      )
    )
;; ---------------------------------------------------------------------------------------------------
  )
