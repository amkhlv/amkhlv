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
          scribble/extract
          (for-label racket  
                     scribble/core 
                     scribble/base 
                     scribble/html-properties 
                     scribble/decode
                     scriblib/render-cond
                     (prefix-in the: xml)
                     xml/path
                     bystroTeX/common
                     "truques.rkt"
                     "xml.rkt"
                     "sqlite.rkt"
                     "terminal.rkt"
                     ))
@(require bystroTeX/common "truques.rkt")

@title{Various tools}

Here I will write up various tricks useful with Scribble. 


@section{Truques}
@defmodule[truques/truques]

@defform[(explain expr ...)]{
First prints the code, then executes it an prints the result of the execution.
}

@defproc[
(show-and-go
 [a namespace-anchor?]
 [#:rest xs (listof string?)] 
) 
block?
]{
@bold{Deprecated}, better use @racket{(explain xs ...)}.
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

@defproc[
(mailto [#:rest ems (listof string?)])
element?
]{Inserts the @tt{mailto:} link}

@defproc[
(autolist [#:exts list-of-extensions (listof symbol?) '(pdf)]
          [#:dir dir path-string? (get-bystro-scrbl-name)]
          [#:header header (or/c (listof any/c) #f) #f]
          [#:output o (-> path-string? (or/c (listof any/c))) (λ (p)   `(,(hyperlink (path->string (path->complete-path (build-path (get-bystro-scrbl-name) p))) (path->string p))))]
          [#:filter f (-> path-for-some-system? boolean?) (λ (p) #t)]
          )
table?
]{Lists files with given extensions in the directory.
Each file is listed as a table row generated by @tt{f} }

@defproc[
(autolist-pdfs [#:dir dir path-string? (get-bystro-scrbl-name)]
               [#:showtime st boolean? #f]
               [#:filter f (-> path-for-some-system? boolean?) (λ (p) #t)]
               )
(or/c table? element?)
]{
Lists PDF files is the directory.
}

@defproc[
(autolist-images [#:exts list-of-extensions (listof symbol?) '(svg png tiff jpg jpeg)]
                 [#:dir dir path-string? 'same]
                 [#:scale scale number? 0.25]
                 [#:ncols number-of-columns integer? 2]
                 [#:filter filt (-> path-for-some-system? boolean?) (λ (p) #t)]
                 [#:showtime st boolean? #f]
                 [#:showdir sd boolean? #t]
                 [#:output o (path-string?  path? . -> . block?) (λ (d f)
                       (tbl `(,@`((,(hyperlink
                                     (build-path d f)
                                     (image #:scale scale (build-path d f))))
                                  (,(path->string f)))
                              ,@(if st
                                    `((,(date->string
                                         (seconds->date
                                          (file-or-directory-modify-seconds
                                           (find-relative-path
                                            (current-directory)
                                            (path->complete-path (build-path d f))))))))
                                    '()))))]
                 )
(or/c nested-flow? element?)
]{
List image files in the directory
}
             
@defproc[
(autolist-svgs
 [#:dir dir path-string? 'same]
 [#:scale scale number? 0.25]
 [#:ncols number-of-columns integer? 2]
 [#:filter filt (-> path-for-some-system? boolean?) (λ (p) #t)]
 [#:showtime st boolean? #f]
 [#:showdir sd boolean? #t]
 [#:annotated annot boolean? #f]
 )
(or/c nested-flow? element?)
]{
List SVG files in the directory.
When @racket[annotated] is @racket[#t], scans SVG files for the
@tt{<desc>} tag and collects, on top of the table, the links to the corresponding images. In @bold{Inkscape}, the @tt{desc} can be inserted by going to @tt{Object properties}
and filling the @tt{Description} line. This is useful for creating a ``tag cloud''.
}

@section{Text}

@defproc[
(copy-to-clipboard
 [#:rows rows string? #f]
 [#:cols cols string? #f]
 [xs (listof string?)])
(block?)
]{
Content is copied to clipboard on pressing the button
}

@defproc[
(check [xs (listof string?)])
(element?)
]{Checkbox labelled by the text}

@subsection{Nested styles}
@verb|---{
@nested[#:style @(make-style "comment" '()) @nested[#:style @(make-style "greenbox" '()) @verb|-{ ... @verb}-|
}---|

@subsection{Align to right}
@(define-namespace-anchor b)
@verb|--{
(tg table #:attrs ([border "0"] [width "100%"])
    (tg tr (tg td #:attrs ([style "text-align:right;"])
               "First line"
               (linebreak)
               "Second line"
               )))
}--|
@(tg table #:attrs ([border "0"] [width "100%"])
    (tg tr (tg td #:attrs ([style "text-align:right;"])
               "First line"
               (linebreak)
               "Second line"
               )))


@section{Format}
This prints @tt{x} to four decimal places:
@verb|--{
@(require racket/format)
(~r #:precision '(= 4) x)
}--|

@section{XML}
@defmodule[truques/xml]

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

@defparam[
transform-to-content h (hash/c symbol? (-> the:xexpr? content?))]{
A parameter that defines the currently default transformers to content argument in @racket[show-xexpr]
}

@defparam[
transform-to-block h (hash/c symbol? (-> the:xexpr? block?))]{
A parameter that defines the currently default transformers to block argument in @racket[show-xexpr]
}

@defproc[
(show-xexpr [x the:xexpr?] 
            [#:transform-to-content t (hash/c symbol? (-> the:xexpr? content?)) (transform-to-content)]
            [#:transform-to-block tblock (hash/c symbol? (-> the:xexpr? block?)) (transform-to-block)]
            [#:show-root sr boolean? #f]
            [#:size size (or/c integer? boolean?) #f]
            [#:size-step step number? 0.93]
            [#:steps steps integer? 4])
(or/c #f content? block?)
]{
Show XML data as a table. Here @racket[size] is the size of the root, and 
@racket[steps] is the  number of size-decreasing steps before size stabilization.

Example:
@verb|--{
@(define (f-transformer e) (f+0-2 (se-path* '(f) e)))
@(define (e-transformer email) 
   (hyperlink 
    (string-append "mailto:" (se-path* '(email) email)) 
    (se-path* '(email) email)))
@(parameterize
     ([transform-to-content 
       (hash-set* (transform-to-content) 
                  'f f-transformer 
                  'email e-transformer)])
   (show-xexpr some-xexpr))
}--|
}


@defproc[
($->_ [x string?]) 
string?
]{
Replaces dollar sign with underscore. Useful for SQLITE queries.
}

@section{SQLite tables}
@defmodule[truques/sqlite]

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



@section{Terminal}
@defmodule[truques/terminal]

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

@section{Dhall}

@(include-extracted truques/dhall)

@section{DOCX}

@(include-extracted truques/docx)


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
