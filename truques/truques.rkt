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

(module truques racket
  (require scribble/core scribble/base scribble/html-properties scribble/decode scriblib/render-cond racket/string)
  (require bystroTeX/common)

  (define copy-tag-num 0)

  (provide (contract-out [show-and-go (->* (namespace-anchor?) () #:rest (listof string?) block?)]))
  (define (show-and-go a . x)
    (define thisns (namespace-anchor->namespace a))
    (let ((mycode (apply string-append x)))
        (nested (nested #:style (style "comment" '()) (verb mycode))
                (eval (read (open-input-string (string-append "(begin " mycode ")"))) thisns))))

  (provide (contract-out [curdir (-> element?)]))
  (define (curdir)
                                        ;  Inserts the link to the current dir
    (hyperlink (bystro-path-to-link ".") 
               #:style (make-style 
                        "sourcelink" 
                        (list (make-css-addition "misc.css"))) 
               "*dir*"))
  (provide (contract-out [mailto (->* () () #:rest (listof string?) element?)]))
  (define (mailto . x)
    (let* ([xx (map 
                (λ (u) (string-trim u #px"\\<|\\>")) 
                (filter 
                 ((curry regexp-match?) #rx"@") 
                 (apply append (map string-split x))))]
           [z (apply 
               ((curry string-append) "mailto:")
               (add-between xx ","))])
      (hyperlink z (add-between (filter (compose not ((curry regexp-match?) #px"^\\s*$")) x) " ⋄ "))))

  (provide (contract-out [copy-to-clipboard (->* () () #:rest (listof string?) element?)]))
  (define (copy-to-clipboard . xs)
    (set! copy-tag-num (+ 1 copy-tag-num))
    (element
     (style #f '())
     (list
      (tg
       textarea
       #:attrs ([id (string-append "amkhlv-bystro-copy-id-" (number->string copy-tag-num))]
                [readonly "1"])
       (apply string-append xs)
       )
      (tg
       button
       #:attrs ([onclick (string-append "amkhlvBystroCopyFn" (number->string copy-tag-num) "()")])
       "COPY")
      (tg
       script
       (string-append
        "function amkhlvBystroCopyFn"
        (number->string copy-tag-num)
        "() {var copyText = document.getElementById(\""
        "amkhlv-bystro-copy-id-"
        (number->string copy-tag-num)
        "\") ; copyText.select(); document.execCommand(\"copy\"); }"
        )
       )
      )
     )
    )
  )
