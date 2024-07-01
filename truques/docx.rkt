#lang at-exp racket

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


(require racket/base (only-in xml xexpr/c xexpr->string))
(require scribble/srcdoc (for-doc scribble/base scribble/manual))
(require
 (only-in scribble/core element paragraph content? block? style)
 (only-in scribble/base hyperlink nested)
 (only-in scribble/html-properties attributes make-css-addition)
 (only-in scribble/manual racket)
 )

(require "xml.rkt")
(require (only-in bystroTeX/common bystro-path-to-link))


(struct a-bold (contents))
(provide (proc-doc b (->i () () #:rest [txt rich-text/c] [result rich-text-element/c]) ()))
(define (b . x) (a-bold x))
(struct a-italic (contents))
(provide (proc-doc i (->i () () #:rest [txt rich-text/c] [result rich-text-element/c]) ()))
(define (i . x) (a-italic x))
(struct a-underline (contents))
(provide (proc-doc u (->i () () #:rest [txt rich-text/c] [result rich-text-element/c]) ()))
(define (u . x) (a-underline x))
(struct a-link (href text))
(provide (proc-doc a (->i ([href string?] [txt string?]) ()  [result rich-text-element/c]) ("hyperlink")))
(define (a h t) (a-link h t))
(struct a-image (src caption))
(provide (proc-doc img (->i ([src path-string?] [caption string?]) ()  [result rich-text-element/c]) ("image")))
(define (img s c) (a-image s c))
(define rich-text-element/c (or/c string? a-bold? a-italic? a-underline? a-link? a-image?))
(define rich-text/c (listof rich-text-element/c))
(define (ins a as)
  (if (member (car a) (map car as)) as (cons a as)))
(define (prepattr attrs)
  (let ([x (filter cadr attrs)])
    (if (empty? x) '() `(,x))))
(provide (proc-doc
          p
          (->i
           ()
           (#:size [size  (or/c string? #f)]
            #:color [color  (or/c string? #f)]
            #:align [align  (or/c string? #f)]
            )
           #:rest [rt rich-text/c]
           [result xexpr/c])
          ([size #f] [color #f] [align #f])
          ("paragraph builder")))
(define (p #:size [sz #f] #:color [clr #f] #:align [aln #f]
           . exprs)
  (append
   `(p ,@(prepattr `([size ,sz] [color ,clr] [align ,aln])))
   (append-map
    (λ(expr)
      (let rec ([attrs '()] [x expr])
        (match x
          [str
           #:when (string? str)
           `((r ,@(prepattr attrs) ,str))]
          [(a-link h t)
           `((a ([href ,h]) ,t))]
          [(a-image s c)
           `((img ([src ,s]) ,c))]
          [(a-bold xs)
           (append-map (λ (e) (rec (ins '(b "") attrs) e)) xs)]
          [(a-italic xs)
           (append-map (λ (e) (rec (ins '(i "") attrs) e)) xs)]
          [(a-underline xs)
           (append-map (λ (e) (rec (ins '(u "") attrs) e)) xs)]
          )
        )
      )
    exprs)
   )
  )
(provide (proc-doc
          t
          (->i () () #:rest [row (listof xexpr/c)] [result xexpr/c])
          ("table")))
(define (t . rows) `(table ,@rows))
(provide (proc-doc
          tr
          (->i () () #:rest [cell (listof xexpr/c)] [result xexpr/c])
          ("table row")))
(define (tr . cells) `(tr ,@cells))
(provide (proc-doc
          td
          (->i () () #:rest [content (listof xexpr/c)] [result xexpr/c])
          ("table cell")))
(define (td . contents) `(td ,@contents))

(provide (proc-doc
          docx-xexpr
          (->i () () #:rest [paragraph-or-table (listof xexpr/c)] [result xexpr/c])
          ("build document as xexpr (actually just (root expr...))")))
(define (docx-xexpr . expr) `(root ,@expr))

(provide (proc-doc
          docx->file
          (->i ([file path-string?] [doc-as-xexpr  xexpr/c]) () [result void?])
          ("save docx-xexpr to file")))
(define (docx->file file-path doc-xexpr)
  (let-values
      ([(proc out in err)
        (subprocess #f #f #f (find-executable-path "xml2docx") "-o" file-path)])
    ;; (with-output-to-file
    ;;   #:exists 'replace
    ;;   "eraseme.xml"
    ;;   (λ () (display (xexpr->string doc-xexpr))))
    (display (xexpr->string doc-xexpr) in)
    (close-output-port in)
    (display (port->string err) (current-error-port))
    (close-input-port err)
    (close-input-port out)
    ))

(provide (proc-doc
          docx-here
          (->i
           ([file path-string?])
           ()
           #:rest [line  (or/c (cons/c 'frozen any/c) (listof xexpr/c))]
           [result (or/c #f content? block?)]
           )
          @{
             Prepare DOCX file and show it here.

             If the content starts with @racket['frozen], then the rest is simply ignored
             and no new docx file is generated. 

             A root child is either @racket[(p . x)] or (@racket[t]...) (that is, paragraph or table)

             A paragraph child is either string or @racket[(b . x)] or @racket[(i . x)] or @racket[(a url txt)] or @racket[(img src caption)]
            }))
(define (docx-here file-path . line)
  (let ([banner
         (paragraph
          (style
           "docx-file-link-paragraph"
           `(,(attributes `(,(cons 'align "right")))))
          (hyperlink
           #:style
           (style "docx-file-link" '())
           (bystro-path-to-link file-path) file-path))
         ])     
    (if (eq? (car line) 'frozen)
        (nested banner #:style (style "docx" '()))
        (let ([rooted `(root ,@line)])
          (docx->file file-path rooted)
          (nested
           banner
           (show-docx rooted)
           #:style (style "docx" '())
           )))))


