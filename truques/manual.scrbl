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
                     (planet amkhlv/bystroTeX/common)
                     "truques.rkt" 
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
