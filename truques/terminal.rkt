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

(module common racket
  (require bystroTeX/utils)

  (provide stty-minus-f-arg-string)
  (define stty-minus-f-arg-string
    (case (system-type 'os)
      ((macosx) "-f")
      (else "-F")))

  (provide askpass)
  (define (askpass)
    (define stty-orig
      (with-external-command-as 
       stty ("stty" stty-minus-f-arg-string "/dev/tty" "-g")
       (let ([x (read-line stty-stdout)]
             [e (port->lines stty-stderr)]
             )
         (for ([l e]) (display l) (display "  <--- error!\n"))
         x
         )))
    (with-external-command-as
     stty ("stty" stty-minus-f-arg-string "/dev/tty" "-echo")
     (let (
           [e (port->lines stty-stderr)]
           )
       (for ([l e]) (display l) (display "  <--- error!\n"))
       ))
    (define passphrase (read-line))
    (with-external-command-as
     stty ("stty" stty-minus-f-arg-string "/dev/tty" stty-orig)
     (let (
           [e (port->lines stty-stderr)]
           )
       (for ([l e]) (display l) (display "  <--- error!\n"))
       ))
    passphrase)

  (provide get-one-char)
  (define (get-one-char)
    (define stty-orig
      (with-external-command-as 
       stty ("stty" stty-minus-f-arg-string "/dev/tty" "-g")
       (let ([x (read-line stty-stdout)]
             [e (port->lines stty-stderr)]
             )
         (for ([l e]) (display l) (display "  <--- error!\n"))
         x
         )))
    (with-external-command-as
     stty ("stty" stty-minus-f-arg-string "/dev/tty" "-echo" "raw")
     (let (
           [e (port->lines stty-stderr)]
           )
       (for ([l e]) (display l) (display "  <--- error!\n"))
       ))
    (define onechar (read-char))
    (with-external-command-as
     stty ("stty" stty-minus-f-arg-string "/dev/tty" stty-orig)
     (let (
           [e (port->lines stty-stderr)]
           )
       (for ([l e]) (display l) (display "  <--- error!\n"))
       ))
    onechar)


  (provide ansi-off)
  (define ansi-off "\033[0m")
  (provide ansi-fg256)
  (define (ansi-fg256 rgb x) 
    (format "\033[38;5;~am~a~a" rgb x ansi-off))
  (provide ansi-bg256)
  (define (ansi-bg256 rgb x) 
    (format "\033[48;5;~am~a~a" rgb x ansi-off))
  (provide ansi-bold)
  (define (ansi-bold x)
    (format "\033[1m~a~a" x ansi-off))
  (provide ansi-underline)
  (define (ansi-underline x)
    (format "\033[4m~a~a" x ansi-off))   
  (provide ansi-blink)
  (define (ansi-blink x)
    (format "\033[5m~a~a" x ansi-off))   
  (provide ansi-reverse)
  (define (ansi-reverse x)
    (format "\033[7m~a~a" x ansi-off))   
  (provide ansi-clear-screen)
  (define (ansi-clear-screen)
    (display "\033[2J"))
  )
