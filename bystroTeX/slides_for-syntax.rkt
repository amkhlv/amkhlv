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
                                 #:calc-align               
                                 [calc-align
                                  (lambda (s) `(+ (bystro-manual-base-alignment bystro-conf) ,s))]
                                 #:calc-size
                                 [calc-size
                                  (lambda (s) `(+ (bystro-formula-size bystro-conf) ,(* 2 s)))]
                                 stx)
    (let* (
           [formula-db `(define formula-database
                          (begin 
                            (configure-bystroTeX-using bystro-conf)
                            (bystro-initialize-formula-collection bystro-conf)))]
           [formula-proc `(unless (bystro-formula-processor bystro-conf)
                            (error "*** could not find executable for formula processing ***"))]
           [auto `(define ,(string->symbol auto-prefix) 
                    (lambda u (bystro-formula #:use-depth #t (apply string-append u))))]
           [disp `(define ( ,(string->symbol display-math-prefix) 
                            #:label (lbl #f) 
                            #:size (n (bystro-formula-size bystro-conf)) 
                            . x)
                    (bystro-equation #:label lbl #:size n x))]
           [old-formula-size      (string->unreadable-symbol "oldfsize")]
           [old-autoalign-adjust  (string->unreadable-symbol "old-aa-adjust")]
           [oldsz `(define ,old-formula-size (bystro-formula-size bystro-conf))]
           [oldaa `(define ,old-autoalign-adjust (bystro-autoalign-adjust bystro-conf))]
           [ch-sz `(define (,(string->symbol size-change-id) (i #f) (aaadj #f))
                     (if i (begin 
                             (set! ,old-formula-size (bystro-formula-size bystro-conf))
                             (set-bystro-formula-size! bystro-conf i)
                             (when aaadj (begin 
                                           (set! ,old-autoalign-adjust 
                                                 (bystro-autoalign-adjust bystro-conf))
                                           (set-bystro-autoalign-adjust! bystro-conf aaadj))))
                         (begin 
                           (set-bystro-formula-size! bystro-conf ,old-formula-size)
                           (set-bystro-autoalign-adjust! bystro-conf ,old-autoalign-adjust)
                           )))]
           [inc-sz `(define (,(string->symbol size-increase-id) i (aaadj #f))
                      (set! ,old-formula-size (bystro-formula-size bystro-conf))
                      (set-bystro-formula-size! bystro-conf (+ (bystro-formula-size bystro-conf) i))
                      (when aaadj 
                        (begin (set! ,old-autoalign-adjust 
                                     (bystro-autoalign-adjust bystro-conf))
                               (set-bystro-autoalign-adjust! bystro-conf aaadj))))]
           [rs-sz  `(define (,(string->symbol size-restore-id))
                      (set-bystro-formula-size! bystro-conf ,old-formula-size)
                      (set-bystro-autoalign-adjust! bystro-conf ,old-autoalign-adjust)
                      )]
           [fname  `(register-path-to-scribble-file (syntax-source #`stx))]
           [ttp-init '(bystro-titlepage-init #:singlepage-mode singlepage-mode)]
           [l+ (lambda (m)
                 `(define 
                    (,(string->symbol (format "~a+~a" formula-prefix m)) . u)
                    (bystro-formula #:align ,(calc-align  m) 
                                    (apply string-append u))))]
           [l- (lambda (m)
                 `(define 
                    (,(string->symbol (format "~a-~a" formula-prefix m)) . u)
                    (bystro-formula #:align ,(calc-align (-  m)) 
                                    (apply string-append u))))]
           [l++ (lambda (m z)
                  `(define 
                     (,(string->symbol (format "~a+~a+~a" formula-prefix m z)) . u)
                     (bystro-formula #:align ,(calc-align m) 
                                     #:size  ,(calc-size z)
                                     (apply string-append u))))]
           [l+- (lambda (m z)
                  `(define 
                     (,(string->symbol (format "~a+~a-~a" formula-prefix m z)) . u)
                     (bystro-formula #:align ,(calc-align m) 
                                     #:size  ,(calc-size (- z))
                                     (apply string-append u))))]
           [l-+ (lambda (m z)
                  `(define 
                     (,(string->symbol (format "~a-~a+~a" formula-prefix m z)) . u)
                     (bystro-formula #:align ,(calc-align (- m)) 
                                     #:size  ,(calc-size z)
                                     (apply string-append u))))]
           [l-- (lambda (m z)
                  `(define 
                     (,(string->symbol (format "~a-~a-~a" formula-prefix m z)) . u)
                     (bystro-formula #:align ,(calc-align (- m)) 
                                     #:size  ,(calc-size (- z)) 
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
                         formula-proc
                         auto 
                         disp 
                         oldsz 
                         oldaa 
                         ch-sz 
                         inc-sz 
                         rs-sz 
                         fname
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
