#|
Copyright 2015 Andrei Mikhailov

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

(module utils racket

  (require racket/system (for-syntax racket/syntax))

  (provide process*)

  (provide (contract-out [close-these-ports: (->* () #:rest (listof (or/c input-port? output-port?)) void?)]))
  (define (close-these-ports: . ps)
    (for ([p ps] #:when p)
      (cond
       [(input-port? p) (close-input-port p)]
       [(output-port? p) (close-output-port p)])))
  (define (stringify x)
    (if (symbol? x) (symbol->string x) x))

  (provide with-external-command-as)
  (define-syntax (with-external-command-as stx)
    (syntax-case stx ()
      [(_ p-nick (com arg ...))
       (with-syntax 
           ([nick-stdout (format-id stx "~a-stdout" #'p-nick)]
            [nick-stdin  (format-id stx "~a-stdin"  #'p-nick)]
            [nick-pid    (format-id stx "~a-pid"    #'p-nick)]
            [nick-stderr (format-id stx "~a-stderr" #'p-nick)]
            [nick-ctrl   (format-id stx "~a-ctrl"   #'p-nick)]
            )
         #`(let* 
               ([p-params (process* (find-executable-path (stringify com)) arg ...)]
                [nick-stdout (car p-params)]
                [nick-stdin  (cadr p-params)]
                [nick-pid    (caddr p-params)]
                [nick-stderr (cadddr p-params)]
                [nick-ctrl   (cadr (cdddr p-params))])
             (close-these-ports: nick-stdout nick-stdin nick-stderr)
             ))]
      [(_ p-nick x y action ...) 
       (with-syntax 
           ([nick-stdout (format-id stx "~a-stdout" #'p-nick)]
            [nick-stdin  (format-id stx "~a-stdin"  #'p-nick)]
            [nick-pid    (format-id stx "~a-pid"    #'p-nick)]
            [nick-stderr (format-id stx "~a-stderr" #'p-nick)]
            [nick-ctrl   (format-id stx "~a-ctrl"   #'p-nick)]
            )
         (syntax-case #'x ()
           [(com arg ...)
            #`(let* 
                  ([p-params (process* (find-executable-path (stringify com)) arg ...)]
                   [nick-stdout (car p-params)]
                   [nick-stdin  (cadr p-params)]
                   [nick-pid    (caddr p-params)]
                   [nick-stderr (cadddr p-params)]
                   [nick-ctrl   (cadr (cdddr p-params))])
                (define results (let () y action ...))
                (close-these-ports: nick-stdout nick-stdin nick-stderr)
                results)]
           [#:cmdline 
            #`(let* 
                  ([p-params (apply process* (find-executable-path (stringify (car y))) (cdr y))]
                   [nick-stdout (car p-params)]
                   [nick-stdin  (cadr p-params)]
                   [nick-pid    (caddr p-params)]
                   [nick-stderr (cadddr p-params)]
                   [nick-ctrl   (cadr (cdddr p-params))])
                (define results (let () #t action ...))
                (close-these-ports: nick-stdout nick-stdin nick-stderr)
                results)]))]
      ))

  (provide with-subprocess-as)
  (define-syntax (with-subprocess-as stx)
    (syntax-case stx ()
      [(_ p-nick outp inp errp (com arg ...)) 
       (with-syntax 
           ([nick-process (format-id stx "~a-process" #'p-nick)]
            [nick-stdout  (format-id stx "~a-stdout" #'p-nick)]
            [nick-stdin   (format-id stx "~a-stdin"  #'p-nick)]
            [nick-stderr  (format-id stx "~a-stderr" #'p-nick)])
         #`(let-values
               ([(nick-process nick-stdout nick-stdin nick-stderr)
                 (subprocess outp inp errp (find-executable-path (stringify com)) arg ...)])  
             (when nick-stdout (close-input-port nick-stdout))
             (when nick-stdin  (close-output-port nick-stdin))
             (when nick-stderr (close-input-port nick-stderr))))]
      [(_ p-nick outp inp errp x y action ...) 
       (with-syntax 
           ([nick-process (format-id stx "~a-process" #'p-nick)]
            [nick-stdout  (format-id stx "~a-stdout" #'p-nick)]
            [nick-stdin   (format-id stx "~a-stdin"  #'p-nick)]
            [nick-stderr  (format-id stx "~a-stderr" #'p-nick)])
         (syntax-case #'x ()
           [(com arg ...)
            #`(let-values
                  ([(nick-process nick-stdout nick-stdin nick-stderr)
                    (subprocess
                     outp
                     inp
                     errp
                     (find-executable-path (stringify com))
                     arg
                     ...
                     )])  
                (let ([results (let () y action ...)])
                  (when nick-stdout (close-input-port nick-stdout))
                  (when nick-stdin  (close-output-port nick-stdin))
                  (when nick-stderr (close-input-port nick-stderr))
                  results))]
           [#:cmdline 
            #`(let-values
                  ([(nick-process nick-stdout nick-stdin nick-stderr)
                    (apply
                     subprocess
                     `(,outp
                       ,inp
                       ,errp
                       ,(find-executable-path (stringify (car y)))
                       ,@(cdr y)
                       ))])  
                (let ([results (let () #t action ...)])
                  (when nick-stdout (close-input-port nick-stdout))
                  (when nick-stdin  (close-output-port nick-stdin))
                  (when nick-stderr (close-input-port nick-stderr))
                  results))]
           [lst
            #`(let-values
                  ([(nick-process nick-stdout nick-stdin nick-stderr)
                    (apply
                     subprocess
                     `(,outp
                       ,inp
                       ,errp
                       ,(find-executable-path (stringify (car lst)))
                       ,@(cdr lst)
                       ))])  
                (let ([results (let () #t y action ...)])
                  (when nick-stdout (close-input-port nick-stdout))
                  (when nick-stdin  (close-output-port nick-stdin))
                  (when nick-stderr (close-input-port nick-stderr))
                  results))]
           ))]
      [(_ p-nick outp inp errp lst y)
       #`(let-values
             ([(nick-process nick-stdout nick-stdin nick-stderr)
               (apply
                subprocess
                `(,outp
                  ,inp
                  ,errp
                  ,(find-executable-path (stringify (car lst)))
                  ,@(cdr lst)
                  ))])  
       (with-syntax 
           ([nick-process (format-id stx "~a-process" #'p-nick)]
            [nick-stdout  (format-id stx "~a-stdout" #'p-nick)]
            [nick-stdin   (format-id stx "~a-stdin"  #'p-nick)]
            [nick-stderr  (format-id stx "~a-stderr" #'p-nick)])
         (let ([results (let () #t y)])
           (when nick-stdout (close-input-port nick-stdout))
           (when nick-stdin  (close-output-port nick-stdin))
           (when nick-stderr (close-input-port nick-stderr))
           results)))]
      [(_ p-nick outp inp errp lst) #'(with-subprocess-as p-nick outp inp errp lst #t)]
      ))


  (provide run-pipeline)
  (define-syntax (run-pipeline stx)
    (syntax-case stx ()
      [(_ pipe-stdout pipe-stdin (s arg ...))
       #`(let-values 
             ([(process out-inp in-outp err-inp) 
               (subprocess pipe-stdout pipe-stdin (current-error-port) (find-executable-path (stringify s)) arg ...)])
           (when in-outp (close-output-port in-outp))
           out-inp)]
      [(_ pipe-stdout pipe-stdin (s arg ...) (s0 arg0 ...) ...)
       #`(let-values 
             ([(process out-inp in-outp err-inp) 
               (subprocess #f pipe-stdin (current-error-port) (find-executable-path (stringify s)) arg ...)])
           (let ([result (run-pipeline
                          pipe-stdout out-inp
                          (s0 arg0 ...) ...)])
             (when out-inp (close-input-port out-inp))
             (when in-outp (close-output-port in-outp))
             result))]))

  )
