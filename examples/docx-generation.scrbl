#lang scribble/base
@(require racket scribble/core scribble/base scribble/html-properties)
@(require "defs.rkt" bystroTeX/common bystroTeX/slides (for-syntax bystroTeX/slides_for-syntax))
@; ---------------------------------------------------------------------------------------------------
@; User definitions:
@(bystro-set-css-dir (build-path (find-system-path 'home-dir) "a" "git" "amkhlv" "profiles" "writeup"))
@(define bystro-conf   
   (bystro (bystro-connect-to-server (build-path (find-system-path 'home-dir) ".config" "amkhlv" "latex2svg.xml"))
           "docx-generation/formulas.sqlite"  ; name for the database
           "docx-generation" ; directory where to store image files of formulas
           25  ; formula size
           (list 255 255 255) ; formula background color
           (list 0 0 0) ; formula foreground color
           2   ; automatic alignment adjustment
           0   ; manual alignment adjustment
           ))
@(define singlepage-mode #t)
@(bystro-def-formula "formula-enormula-humongula!")

@(require (only-in xml xexpr->string) xml/xexpr truques/docx truques/xml)


@title[#:style '(no-toc no-sidebar)]{DOCX}

@bystro-source[]

@bystro-ribbon[]
@table-of-contents[]





@section{HTML to DOCX using LibreOffice}

@verb|{
soffice --convert-to odt index.html
soffice --convert-to docx index.odt
}|

@section{Via XML using @tt{xexpr} and @tt{xml2docx}}

We can generate rudimentary DOCX documents using
@hyperlink["https://github.com/amkhlv/usr/tree/master/share/Rust/xml2docx"]{xml2docx}.
The XML should satisfy the grammar:
@hyperlink["https://github.com/amkhlv/usr/blob/master/share/Rust/xml2docx/docx.rnc"]{docx.rnc}

Notice that we use @tt{EN SPACE} ( ) to preserve space between runs.

@(docx-here
  "example.docx"
  'frozen 
  @p[#:size "40" #:color "FF0000" #:align "center"]{Famous search engines}
  @t[
     @tr[@td[@p{LexisNexis}] @td[@p{US link is @a["https://www.lexisnexis.com/en-us/search.page"]{here}}]]
     ;alternatively, construct xexpr by hand:
     '(tr (td (p @r{Google})) (td (p @a[([href "https://www.google.com"])]{link})))
     '(tr (td (p @r{Yahoo})) (td (p @r{still exists: }@a[([href "https://search.yahoo.com/"])]{link})))
     ]
  @p[#:size "32"]{@i{Lor@b{i}m Ips@b{e}m etc}}
  @p{line one
          line two
          line three}
  )


@bystro-ribbon[]

@; ---------------------------------------------------------------------------------------------------
@(bystro-close-connection bystro-conf)
@disconnect[formula-database]

  
