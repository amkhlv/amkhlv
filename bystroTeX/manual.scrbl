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
                     racket/system
                     scribble/core 
                     scribble/base 
                     scribble/html-properties 
                     scribble/decode
                     scriblib/render-cond 
                     net/http-client
                     "common.rkt" 
                     "slides.rkt"))

@title{BystroTeX}

@section{Introduction}
This manual is only partially useful, because many functions
are auto-generated by macros. The beginner user should find the instructions
in the form of a slide presentation 
@hyperlink["http://andreimikhailov.com/slides/bystroTeX/slides-manual/index.html"]{here},
and also study the sample file @tt{slides-manual.scrbl}, which is in the
samples directory. That sample file can be used as a template for creating new
presentations. 

As a general rule, those functions whose name starts with @tt{bystro-}, are either
for internal use or for use in the header. They are not meant to be used in slides by themselves,
or at least not to be used frequently. (But this rule is not very strict.)



@section{Functions used in headers}
@defmodule[bystroTeX/slides]
@defstruct[bystroserver  (
                          [connection http-conn?]
                          [token string?]
                          [user (or/c #f string?)]
                          [host string?]
                          [port integer?]
                          [path string?]
                         )
]{Configuration of the LaTeX server}

@defproc[
(bystro-connect-to-server 
 [xmlconf-file (or/c #f path?)]
 )
(or/c 'running-without-LaTeX-server bystroserver?)
]{
Configures the location of the file containing the parameters of the LaTeX server (port number @italic{etc.})
}

@defstruct[bystro (
                   [formula-processor (or/c 'running-without-LaTeX-server bystroserver?)]
                   [formula-database-name path-string?]
                   [formula-dir-name path-string?]
                   [formula-size integer?]
                   [formula-bg-color (list/c (integer-in 0 255) (integer-in 0 255) (integer-in 0 255))]
                   [formula-fg-color (list/c (integer-in 0 255) (integer-in 0 255) (integer-in 0 255))]
                   [autoalign-adjust integer?]
                   [manual-base-alignment integer?]
                  )
]{Basic configuration}

@section{Some general tricks}
To load a stylesheet from a file @tt{filename.css}:

@verbatim|--{
@(element (make-style #f (list (make-css-addition (string->path "filename.css")))) '())
}--|

Example of nesting things:

@verbatim|--{
@nested[ #:style @(make-style "comment" '()) @verb|{
This is some text which I want to be show verbatim
}|
]
}--|

@section{Functions for manipulating slides}
@defmodule[bystroTeX/slides]
@defstruct[bystro ([formula-processor path?]
                   [formula-database-name string?]
                   [formula-dir-name string?]
                   [formula-size integer?]
                   [formula-bg-color (listof integer?)]
                   [formula-fg-color (listof integer?)]
                   [autoalign-adjust integer?]
                   [manual-base-alignment integer?])]{
Configuration structure (mutable)}

@defproc[
(bystro-titlepage-init) 
element?
]{
Installs the titlepage style
}

@defproc[
(slide 
 [x content?]
 [#:tag tg (or/c symbol? string? #f) #f] 
 [#:showtitle sttl boolean? #f]
 [#:rest xs (listof pre-flow?)] 
 )
part?
]{
A basic slide. The title of the slide is @racket[x], and the contents are @racket[xs].
I recommend providing a nice tag @racket[tg], which will serve as a filename for the html.
Otherwize, @racket[x] will be used as a filename, which may lead to awkward effects.
}

@defproc[
(after-pause
 [#:tag tg (or/c symbol? string? #f) #f]
 [#:rest xs (listof pre-flow?)]
 )
part?]{
The continuation of the slide to be shown after pause.
}

@defproc[(remove-slide)
void?]{
Removes the most recently shown part of the slide
}

@defproc[
(page
 [x content?]
 [#:tag tg (or/c symbol? string? #f) #f] 
 [#:showtitle sttl boolean? #f]
 )
pre-part?
]{
This works similar to @racket[slide]. The differences are:
@itemlist[#:style 'ordered
         @item{
               Do not put curved brackets {} around the content.
               The content just goes after @tt|{@page[...]}|, 
               until the next @tt|{@page[...]}|
               }
         @item{
               No way to do @racket[after-pause]
               }
         @item{
               Inside the content,
               use @racket[subpage]
               instead of @racket[section] / @racket[subsection] / @racket[subsubsection]
               }]
The intent is that @racket[page] should be more general-webpage-oriented,
while @racket[slide] more slideshow-oriented.

The CSS style is called @tt{pagetitle}
}

@defproc[
(subpage
 [level integer?]
 [x content?]
 [#:tag tg (or/c symbol? string? #f) #f] 
 )
pre-part?
]{Like  @racket[section] / @racket[subsection] / @racket[subsubsection] , but
inside @racket[page] (and not @racket[slide])

The CSS styles are @tt{pagetitle-N} where @tt{N} is @racket[level]
}


@defproc[
(use-LaTeX-preamble 
 [#:rest xs string?]
 )
void?]{
Defines the LaTeX preamble; this is where we put all our @literal|-{\newcommand{...}{...}}-|s
}

@defproc[
(number-for-formula
 [x string?]
 )
collect-element?]{
Like @literal|{\label{x}}| in @tt{LaTeX}.
}

@defproc[
(ref-formula 
 [x string?]
 )
delayed-element?]{
Like @literal|{\ref{...}}| in @tt{LaTeX}, reference to the label @racket[x].
}

@defproc[
(bystro-bg [r exact-nonnegative-integer?] [g exact-nonnegative-integer?] [b exact-nonnegative-integer?])
void?]{
Set the background color for formulas
}

@defproc[
(bystro-fg [r exact-nonnegative-integer?] [g exact-nonnegative-integer?] [b exact-nonnegative-integer?])
void?]{
Set the foreground color for formulas
}

@defproc[
(bystro-formula 
 [#:database x db? ]
 [#:formulas-in-dir y string? ]
 [#:shell-command z path? #f ]
 [#:size n natural-number/c ]
 [#:bg-color bg-color (listof natural-number/c)]
 [#:fg-color fg-color (listof natural-number/c)]
 [#:align m (or/c (integer-in (- 99) 99) #f) ]
 [#:use-depth ud boolean? ]
 [#:aa-adjust aa-adj (integer-in (- 99) 99) ]
 [#:rest tex (listof string?)])
element?]{
The user probably will not want to call this procedure directly, because there are
various auto-generated shortcuts.
}

@defproc[
(bystro-equation
 [tex (listof string?)]
 [#:size   n  natural-number/c]
 [#:label  l  (or/c string? #f) #f]
 [#:bg-color bg-color (listof natural-number/c) (bystro-formula-bg-color configuration)]
 [#:fg-color fg-color (listof natural-number/c) (bystro-formula-fg-color configuration)]
 )
nested-flow?]{
Display-style formula. But it should not be called directly, because there is
a shortcut defined in the headers of the slide file. 
}

@defproc[
(bystro-toc)
delayed-block?]{
Insert the list of slides. This is for use on the title-slide.
}


@defproc[
(get-bib-from-server [key string?])
hash?]{
Get the @tt{BibTeX} entry from the server.
}

@defproc[
(bystro-dump-LaTeX [b boolean?])
void?]{
This triggers dumping LaTeX formulas instead of insertion of their pictures.
Useful for translating BystroTeX documents into LaTeX.
}

@section{Miscellaneous functions}


@defmodule[bystroTeX/common]

@defproc[(get-bystro-scrbl-filename) string?]{This .scrbl filename}

@defproc[
(bystro-set-css-dir [x path?])
void?]{
Where to find @tt{.css} files. Note that the default @tt{.css} path is the current directory.
}

@defproc[
(bystro-inject-style [#:rest xs (listof string?)])
element?]{
Specify the @tt{.css} files to use. Enter file names. The path is setup by @racket{bystro-set-css-dir},
the default is the current directory.}

@defform[(spn someword content)]{
Returns @tt{<span class="someword">content</span>}. For example:

@tt|-{@span[attn]{remember to pay the phone bill!}}-|. 

Notice the absence of
quotation marks around @tt{attn} in the first argument of @tt|-{@span}-|. The corresponding
CSS could be, for example:
@verbatim|{
.attn {
    color: red;
    font-weight: bold;
}
}|
(that's how it is currently defined in @tt{misc.css})
}

@defform[(div someword content)]{
Similar to @racket[spn] but creates @tt{div} instead of @tt{span}:

 @tt{<div class="someword">content</div>}

The difference is that @tt{div} can extend over several paragraphs.
}

@defform[(tg someword content)]{
Creates @tt{<someword>content</someword>}, for example:
@verbatim|--{
@tg[ol]{
@tg[li]{The union of open sets is an open set.}
@tg[li]{The finite intersection of open sets is an open set.}
@tg[li]{@f{X} and the empty set @f{\emptyset} are open sets.}
}
}--|
Attributes are also supported:
@verbatim|--{
@tg[span #:attrs ([style "color:blue;text-decoration:underline;"] [title "not clickable! Example only!"])]{pseudo-link!!!}
}--|

}

@defform[(init-counter name)]{
This is to set up a counter. For example, suppose that I want to include exercises
in my slides. The exercises should be numbered, Exercise 1, Exercise 2, @italic{etc.}
In the header, I add:
@verbatim|--{
@(require racket/dict)
@(init-counter exercise)
@(define (ex-num label)
   (elemtag label (exercise-next label)))
@(define (ex-ref label)
   (elemref label (list "Exercise " (exercise-number label))))
}--|
Then, when I want to give the next Exercise, I say:
@verbatim|--{
@bold{Exercise @ex-num{ProvePositivityOfE}:} Show that the energy is always greater thatn zero
}--|
When I need to refer to this exercise later, I say:
@verbatim|--{
As you should have proven in @ex-ref{ProvePositivityOfE}, ...
}--|
}

@defproc[
(tbl [rows bystro-rectangular-table?] [#:orient dirn (or/c 'hor 'vert #f)])
table?
]{
To insert a table
}

@defform[(align alignment-scheme content ...)]{
This is a table-like structure, useful for multiline formulas. Examples:
@verbatim|--{
@align[r.l
 @list[
@f{{2\over 1 - x^2} = }  @f{1+x+x^2 + \ldots +}
]@list[
"" @f{1-x+x^2- \ldots}
]
]
}--|
but this comes out with an alignment defect. A better version:
@verbatim|--{
@align[r.l
 @list[
@f{{2\over 1 - x^2} = }  @v+[2 @f{1+x+x^2 + \ldots +}]
]@list[
"" @f{1-x+x^2- \ldots}
]
]
}--|
Notice that in the first line stands the symbol @racket{r.l} which defines the alignment (right, then left).
The @racket{v+} is a padding, it serves for vertical adjustment, see below.

The @bold{numbered} version of the same formula will be
@verbatim|--{
@align[r.l.n
 @list[
@f{{2\over 1 - x^2} = }  @v+[2 @f{1+x+x^2 + \ldots +}] ""
]@list[
"" @f{1-x+x^2- \ldots} @label{SumOfGeometricProgressions}
]
]
}--|
Notice that the alignment symbol is now @racket[r.l.n], we added letter @racket[n] for the number.
The function @tt|{@label[]}| is defined in the slides' header.
}

@defproc[
(v+ [n exact-nonnegative-integer?] [#:rest xs (listof pre-content?)])
table?
]{
The content is padded from below, therefore it gets shifted up.
}

@defproc[
(v- [n exact-nonnegative-integer?] [#:rest xs (listof pre-content?)])
table?
]{
The content is padded from above, therefore it gets shifted down.
}

@defproc[
(h+ [n exact-nonnegative-integer?] [#:rest xs (listof pre-content?)])
table?
]{
The content is padded from the left, therefore gets shifted to the right.
}

@defproc[
(h- [n exact-nonnegative-integer?] [#:rest xs (listof pre-content?)])
table?
]{
The content is padded from the right, therefore gets shifted to the left.
}


@defproc[
(verb  [x string?] [#:indent i exact-nonnegative-integer?] [#:rest xs (listof string?)])
block?
]{ 
Like @racket[(verbatim x #:indent i xs)] but modified so that copy-past does not introduce extra linebreaks 
}

@defproc[
(clr [colorname string?] [#:rest xs (listof pre-content?)])
element?
]{
Colored text.}

@defproc[
(longtbl
 [          bss          (listof (listof block?))]
 [#:styless stylepropsss (listof (listof (listof (or/c 'left 'right 'center 'top 'baseline 'bottom 'vcenter))))]
 [#:width   w            (integer-in 1 100)])
nested-flow?]{
Produces a tabular structure of width @racket[w] with cell content @racket[bss] and 
cell styles @racket[stylepropsss].
}

@defproc[
(boldred [#:rest x (listof pre-content?)]) 
element?]{
Text in bold red. @bold{Deprecated} in favor of @tt|-{@spn[attn]{x}}-|.
}


@defproc[(bystro-js [x string?]) 
traverse-element?
]{Javascript injection}

@defproc[(bystro-js-url [url string?])
traverse-element?
]{Insert Javascript from URL}

@defproc[
(bystro-elemstyle [x (or/c #f string?)] [#:rest otherprops (listof any/c)]) 
style?
]{Style selector for element. Example of use: 

@racket[@element[@bystro-elemstyle{vertical-align:middle} @image{flowers-on-the-wall.png}]]

The @racket[otherprops] are as described in the manual of @racket[element]
}

@defproc[
(bystro-rectangular-table? [l any]) 
boolean?
]{Verifies if @racket[l] is a list of lists representing a rectangular table}


@defproc[
(bystro-path-to-link [x string?]) 
string?
]{Returns the string representing a file URL for the UNIX path represented by @racket[x]}


@defproc[
(bystro-dir-contains-scrbl?
 [p path?])
boolean?]{
Returns @racket[#t] if the directory @racket[p] contains a scribble-file
}

@defproc[
(bystro-list-scrbls
 [p path?] 
 [#:exclude-same-name x boolean? #t])
(listof path?)]{
Return the list of paths to scribble files in the dir @racket[p]; if @racket[x] is @racket[#t], then exclude the
file which has the same name as the source file
}

@defproc[
(bystro-list-scrbls-in-dir
 [p path?]
 [#:background-color clr (listof integer?) '(251 206 177)]
)
element?]{
A nicely formatted list of links to scribble files in the folder @racket[p]
}

@defproc[
(bystro-ribbon)
table?
]{
Formatted list of all the @tt{.scrbl} files (more precisely, the links to the corresponding @tt{.html} files) 
in the current directory.
}

@defproc[
(bystro-ribbon-for-location 
 [p path?]
 [#:exclude-same-name esn boolean? #f]
 )
block?
]{
Same as @racket[bystro-ribbon] but for some other directory (instead of the current directory).
}

@defproc[
(bystro-shell-dump
 [command string?]
 [#:stdin stdin (or/c (and/c input-port? file-stream-port?) #f) #f] 
 [#:style style (or/c style? #f) #f]
 [#:indent indent exact-nonnegative-integer? 0]
 [#:rest arguments (listof string?)]
 )
block?]{
Inserts the output of a shell command. For example:
@verbatim|--{
@bystro-shell-dump|{sqlite3 -line base.sqlite
                    select capital,population from countries where continent like '%asia%'}|
Note how we use the at-syntax instead of quotation marks in the SQL query
}--|
}

@defproc[
(bystro-get-cl-argument
 [key string?]
 )
string?]{
Extract the value of command line argument of the @tt{scribble} command. For example,
if we invoke as follows:
@verbatim|--{
scribble ++arg --person ++arg Andrei filename.scrbl
}--|
then @racket{(bystro-get-cl-argument "person")} will return @racket{"Andrei"}.
} 


@section{Functions for reading the configuration file}
@defmodule[bystroTeX/xmlconf]

The configuration file is usually called @tt{bystrotex.xml}

@defproc[
(xml-file->bystroconf-xexpr
 [xf path-string?]
 )
xexpr?
]{
Reads configuration an XML file
}

@defthing[bystroconf-xexpr (or/c xexpr? #f)]{
Configuration read from the file @tt{bystrotex.xml} in the current directory.
(If there is no such file, returns @racket[#f]
}

@defthing[all-names (listof string?)]{
List of all scribbling names in the current directory
}

@defproc[
(get-bystroconf 
 [name string?]
 )
(or/c xexpr? #f)
]{
Returns the configuration for @racket[name]
}

@defform[
(with-bystroconf 
  bc 
  (name dest name.html name.scrbl formulas/ .sqlite arglist multipage?) 
  body ...)
]{
Here @racket[bc] is a configuration for a scribbling (typically a result of @racket[get-bystroconf]).
In the body, @racket[name] (@racket[string?]) gets bound to @tt{name}, @racket[dest] (@racket[string?]) to @tt{dest},
@racket[name.html] (@racket[path?]) to just @tt{name.html}, @racket[name.scrbl] (@racket[path?]) to just @tt{name.scrbl},
@racket[formulas/] (@racket[string?]) to the directory where SVG files of formulas will be stored, 
@racket[.sqlite] (@racket[string?]) to the path to SQLite file.
(You can provide your own set of identifiers.)
}



@section{Auxiliary functions}

@defmodule[bystroTeX/utils]

@defform[(with-external-command-as nick (com arg ...) action ...)]{
A wrapper around the Racket's @racket[process*] procedure. It executes
@tt{(find-executable-path com)} to find the executable. The following identifiers are
available:
@itemlist[
@item{@tt{nick-stdout} is the @racket[input-port] from the @tt{stdout} of the process}
@item{@tt{nick-stdin} is the @racket[output-port] to the @tt{stdin} of the process}
@item{@tt{nick-pid} is the PID}
@item{@tt{nick-stderr} is the @racket[input-port] from the @tt{stderr} of the process}
@item{@tt{nick-ctl} is the control procedure, see the manual for @racket[process*]}
]
}

@defform[(with-external-command-as nick #:cmdline xs action ...)]{
An alternative form, where @racket[xs] is @racket[(list cmd arg ...)]
}

@defform[(with-subprocess-as nick outp inp errp (com arg ...) action ...)]{
A wrapper around the Racket's @racket[subprocess] procedure. It executes
@tt{(find-executable-path com)} to find the executable. The following identifiers are
available:
@itemlist[
@item{@tt{nick-process} is the resulting @racket[subprocess]}
@item{@tt{nick-stdout} is the @racket[input-port] from the @tt{stdout} of the process}
@item{@tt{nick-stdin} is the @racket[output-port] to the @tt{stdin} of the process}
@item{@tt{nick-stderr} is the @racket[input-port] from the @tt{stderr} of the process}
]
See the manual for @racket[subprocess]
}

@defform[(run-pipeline pipe-stdout pipe-stdin (com arg ...) ...)]{
Runs the pipeline and returns the @racket[input-port] from its @tt{stdout}.
(Which should eventually be closed by calling @racket[close-input-port] !)
}


@section{Legal}

Copyright 2012-2016 Andrei Mikhailov

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
