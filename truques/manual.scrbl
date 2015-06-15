#lang scribble/doc

@;{
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
}

@(require scribble/manual
          (for-label racket  
                     scribble/core 
                     scribble/base 
                     scribble/html-properties 
                     scribble/decode
                     scriblib/render-cond
                     (prefix-in the: xml)
                     xml/path
                     (planet amkhlv/bystroTeX/common)
                     "truques.rkt"
                     "xml.rkt"
                     "sqlite.rkt"
                     "terminal.rkt"
                     ))
@(require (planet amkhlv/bystroTeX/common))
@title{Truques}

Here I will write up various tricks useful with Scribble. 

@section{Format}
This prints @tt{x} to four decimal places:
@verb|--{
@(require racket/format)
(~r #:precision '(= 4) x)
}--|

@section{XML}
@defmodule[(planet amkhlv/truques/xml)]

@defproc[
(file->xexpr [a path-string?]) 
the:xexpr?
]{Reads the @racket[the:xexpr] from the XML file}

@defproc[
(xexprs->nested-flow [xs (listof the:xexpr?)] [#:style s any/c #f])
nested-flow?
]{concatenates the values of XML nodes into a nested flow}

@verb|--{
@(let ([x (file->xexpr  "FILENAME.xml")])
   (tbl #:orient 'hor
        (for/list ([p (se-path*/list '(people) x)] #:when (cons? p)) 
          `(,(se-path* '(person #:nick) p)
            ,(xexprs->nested-flow #:style (bystro-elemstyle "background-color: LightCyan;") (se-path*/list '(person) p))))))
}--|

Why did not we write simply:
@verb|--{
(se-path* '(person) p)  (WRONG!)  ?
}--|
Because sometimes we have: @tt|--{ <person>&amp;john</person> }--| (can we exclude that a person's name starts with an ampersand?)

In XML, to insert the newline use @tt{&#a;}, and to insert the space use @tt{&#a0;}.

@section{YAML}
@verb|--{
@(require (planet esilkensen/yaml))
@(define yaml-dict
   (let*
       ((in (open-input-file "filename.yaml"))
        (yml (read-yaml in))
        )
     (close-input-port in)
     yml))
@(tt (hash-ref yaml-dict "keyname"))
}--|

@section{Nested styles}
@verb|---{
@nested[#:style @(make-style "comment" '()) @nested[#:style @(make-style "greenbox" '()) @verb|-{ ... @verb}-|
}---|

@section{SQLite tables}
@defmodule[(planet amkhlv/truques/sqlite)]

Start with defining the database connection:

@verb|--{
@(define conn (sqlite3-connect #:database "/path/to/your/database.sqlite" #:mode 'read-only))
}--|

And in the end do not forget to disconnect:

@verb|--{
@(disconnect conn)
}--|



@defproc[
(mysqli-tables 
 [conn connection?] 
 [#:column-titles column-titles (listof string?)]
 [#:sql x string?] 
 [#:params params (listof string?) '()]
 [#:css css-file path-string?]
 [#:to-highlight to-hl (listof string?) '()]
 [#:to-hide  to-hide     (listof string?) '()])
(listof block?)]{
Prints out an SQLite database. The parameters @racket[to-highlight] and @racket[to-hide] are column titles as specified in 
@racket[column-titles]. Notice that @racket[column-titles] do not have to be same as actual names of columns in the database.
They are supposed to be ``titles'' for nice printing. Their assignment to the column names are by order (in the query).
}

@defproc[
(get-column-names
 [conn connection?] 
 [#:table t string?])
 (listof string?)]{

Get the list of the column names. Sample use:
@verb|--{
@(mysqli-tables
  conn
  #:column-titles (get-column-names conn #:table "mytable")
  #:sql "select * from mytable where name like ?"
  #:params '("Andrei")
  #:css "my-tables-style.css")
}--|
}


@section{Truques}
@defmodule[(planet amkhlv/truques/truques)]

@defproc[
(show-and-go
 [a namespace-anchor?]
 [#:rest xs (listof string?)] 
) 
block?
]{
First prints the code, then executes it an prints the result of the execution. But first need to set up 
a namespace anchor, for example:
@verb|--{
@(define-namespace-anchor a)
@show-and-go[a]|-{
(format "Conversion rate: ~s" (~r #:precision '(= 4) dollar2real))
}-|
}--|
}

@defproc[
(curdir) element?
]{
Inserts link to the current dir 
}

@section{Terminal}
@defmodule[(planet amkhlv/truques/terminal)]

@defproc[(stty-minus-f-arg-string) string?]{returns "-F" for Linux and "-f" for Mac}
@defproc[(askpass) string?]{reads password from the console, without echo}
@defproc[(get-one-char) char?]{waits for keypress}
@defproc[(ansi-off) string?]{gives "\\033[0m"}
@defproc[(ansi-fg256 [rgb integer?] [x string?]) string?]{256-color terminal, 
see @hyperlink["https://en.wikipedia.org/wiki/File:Xterm_256color_chart.svg"]{Xterm_256color_chart.svg}}
@defproc[(ansi-bg256 [rgb integer?] [x string?]) string?]{256-color terminal}
@defproc[(ansi-bold [x string?]) string?]{bold}
@defproc[(ansi-underline [x string?]) string?]{underlined}
@defproc[(ansi-blink [x string?]) string?]{blinking}
@defproc[(ansi-reverse [x string?]) string?]{reverse}
@defproc[(ansi-clear-screen) any/c]{clear screen}

@section{Legal}

Copyright 2012,2013,2014 Andrei Mikhailov

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
