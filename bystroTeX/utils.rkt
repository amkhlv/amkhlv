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
  (provide with-external-command-as)
  (define-syntax (with-external-command-as stx)
    (syntax-case stx ()
      [(_ p-nick com (arg ...) action ...) 
       (with-syntax 
           ([nick-stdout (format-id stx "~a-stdout" #'p-nick)]
            [nick-stdin  (format-id stx "~a-stdin"  #'p-nick)]
            [nick-pid    (format-id stx "~a-pid"    #'p-nick)]
            [nick-stderr (format-id stx "~a-stderr" #'p-nick)]
            [nick-ctl    (format-id stx "~a-ctrl"   #'p-nick)]
            )
         #`(let* 
               ([p-params (process* (find-executable-path com) arg ...)]
                [nick-stdout (car p-params)]
                [nick-stdin  (cadr p-params)]
                [nick-pid    (caddr p-params)]
                [nick-stderr (cadddr p-params)]
                [nick-ctrl   (cadr (cdddr p-params))]
                )
             (define results (begin action ...))
             (close-input-port nick-stdout)
             (close-output-port nick-stdin)
             (close-input-port nick-stderr)
             results
             ))]))
  (provide with-subprocess-as)
  (define-syntax (with-subprocess-as stx)
    (syntax-case stx ()
      [(_ p-nick outp inp errp com (arg ...) action ...) 
       #`(let-values 
             ([(#,(datum->syntax stx (string->symbol (format "~s-process" (syntax->datum #'p-nick))))
                #,(datum->syntax stx (string->symbol (format "~s-stdout" (syntax->datum #'p-nick)))) 
                #,(datum->syntax stx (string->symbol (format "~s-stdin" (syntax->datum #'p-nick)))) 
                #,(datum->syntax stx (string->symbol (format "~s-stderr" (syntax->datum #'p-nick)))))
               (subprocess outp inp errp (find-executable-path com) arg ...)])
           (define results (begin action ...))
           (close-input-port #,(datum->syntax stx (string->symbol (format "~s-stdout" (syntax->datum #'p-nick)))))
           (close-output-port #,(datum->syntax stx (string->symbol (format "~s-stdin" (syntax->datum #'p-nick)))))
           (close-input-port #,(datum->syntax stx (string->symbol (format "~s-stderr" (syntax->datum #'p-nick)))))
           results
           )]))



  )
