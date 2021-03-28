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

(module yaml racket
  (require racket/format yaml)
  (require bystroTeX/common truques/truques)
  (require scribble/core scribble/base scribble/html-properties scribble/decode)
  (require racket/date)

  (provide (all-from-out yaml) (all-from-out racket/format))

  (provide (contract-out [yaml-to-clips (->*
                                         (yaml?)
                                         (#:rows (or/c integer? #f) #:cols (or/c integer? #f))
                                         block?)]))
  (define (yaml-to-clips #:rows [rows 1] #:cols [cols 50] y)
    (match y
      [(list yy ...)
       (apply
        itemlist
        (for/list ([z yy]) (item (yaml-to-clips #:rows rows #:cols cols z))))]
      [(hash-table (k v) ...)
       (tbl
        #:orient 'hor
        (for/list ([z (map cons k v)])
          (list (car z) (yaml-to-clips #:rows rows #:cols cols (cdr z)))))]
      [v
       (copy-to-clipboard #:rows rows #:cols cols v)]
      )
    )
  )
