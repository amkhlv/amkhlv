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


(require racket/base)
;(require bystroTeX/common)
;(require scribble/core scribble/base scribble/html-properties scribble/decode)

(require scribble/srcdoc (for-doc scribble/base scribble/manual))



(provide (proc-doc/names dhall (->* (string? #:dir path-string?) () string?) ((code workdir) ()) ("run Dhall")))
(define  (dhall #:dir workdir code)
  (parameterize
      ([current-directory workdir])
  (let-values
      ([(proc out in err)
        (subprocess #f #f #f (find-executable-path "dhall"))])
    (display code in)
    (close-output-port in)
    (display (port->string err) (current-error-port))
    (close-input-port err)
    (define r (port->string out))
    (close-input-port out)
    r
    )))


