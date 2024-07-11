#|
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
|#

(module common racket
  (require scribble/core scribble/base scribble/html-properties scribble/decode scriblib/render-cond)
  (require setup/dirs)
  (require mzlib/etc)
  (require racket/list)
  (require racket/vector)
  (require racket/path)
  (require "xmlconf.rkt" xml/path)
  (require (for-syntax racket/syntax))
;; ---------------------------------------------------------------------------------------------------
  (define dump-LaTeX? #f)
  (provide dumping-LaTeX?)
  (define dumping-LaTeX? (make-parameter #f))
  (provide bystro-common-dump-LaTeX)
  (define (bystro-common-dump-LaTeX b) (dumping-LaTeX? b))
  (provide bystro-dump-LaTeX-with-$)
  (define bystro-dump-LaTeX-with-$ (make-parameter #t))

;; ---------------------------------------------------------------------------------------------------
  (provide (contract-out
            ; Get the name without extention
            [get-bystro-scrbl-name (-> string?)]))
  (define (get-bystro-scrbl-name) (bystro-get-cl-argument "bystro-name"))
  (provide (contract-out
            ; Get the filename of the scribble file
            [get-bystro-scrbl-filename (-> string?)]))
  (define (get-bystro-scrbl-filename) (format "~a.scrbl" (get-bystro-scrbl-name)))
  (provide (contract-out
            [get-bystro-dest-dir (-> string?)]))
  (define (get-bystro-dest-dir) (bystro-get-cl-argument "bystro-dest"))
           
;; ---------------------------------------------------------------------------------------------------
  (provide (contract-out 
            ; Location of CSS files
            [css-dir path-string?]))
  (define css-dir (build-path 'same))
  (provide (contract-out
                                        ; Set the path to the folder containing the .css files
            [bystro-set-css-dir_common (-> path? void?)]))
  (define (bystro-set-css-dir_common x) (set! css-dir x))
;; ---------------------------------------------------------------------------------------------------
  (provide (contract-out [bystro-inject-style (->* () #:rest (listof string?) element?)]))
  (define (bystro-inject-style . css-file-names)
    (let ((style (make-style 
                  #f
                  (map
                   (λ (fn) (make-css-addition (build-path css-dir (string->path fn))))
                   css-file-names))))
      (make-element style '())))
;; ---------------------------------------------------------------------------------------------------
  (provide (contract-out
                                        ; Provides relative path for e.g.:
                                        ; @(hyperlink (bystro-path-to-link "../document.pdf") "here")
            [bystro-path-to-link (-> path-string? string?)]))
  (define bystro-path-to-link
    (lambda (relpath)
      (string-append "file://" (path->string (path->complete-path (expand-user-path relpath))))))
;; ---------------------------------------------------------------------------------------------------
  (provide spn)
  (define-syntax (spn stx)
    (syntax-case stx ()
      ((_ class content ...)
       #`(element 
             (make-style #,(symbol->string (syntax->datum #'class)) (list)) 
           (list content ...)))))
;; ---------------------------------------------------------------------------------------------------
  (provide div)
  (define-syntax (div stx)
    (syntax-case stx ()
      ((_ class content ...)
       #`(paragraph
          (make-style #,(symbol->string (syntax->datum #'class)) (list 'div)) 
          (list content ...)))))
;; ---------------------------------------------------------------------------------------------------
  (provide tg)
  (define-syntax (tg stx)
    (syntax-case stx ()
      ((_ class #:attrs ([n v] ...) content ...)
       #'(element
             (make-style #f (list (alt-tag (symbol->string (quote class)))
                                  (attributes `(,(cons (quote n) v) ...))))
           (list content ...)))
      ((_ class content ...)
       #'(element 
             (make-style #f (list (alt-tag (symbol->string (quote class)))))
           (list content ...)))
      ))
;; ---------------------------------------------------------------------------------------------------
  (provide (contract-out
                                        ; deep literal
            [deep-literal (-> content? element?)]))
  (define (deep-literal x)
    (if (string? x)
        (literal x)
        (if (element? x) 
            (deep-literal (element-content x))
            (if (list? x)
                (apply elem (map deep-literal x))
                (literal "-- ERROR in applying literal --")))))
  (define (parse-alignment-string x)
    (if (= (string-length x) 0) 
        '()
        (cons 
         (case (string-ref x 0)
           ((#\l) 'left)
           ((#\r #\n) 'right)
           ((#\c) 'center)
           ((#\t) 'top)
           ((#\B) 'baseline)
           ((#\b) 'bottom)
           ((#\v) 'vcenter))
         (parse-alignment-string (substring x 1)))))
  (define (mytab1 x)
    (let ((y (if (block? x) x (para x))))
      (table (style #f '()) (list (list y)))))
  (provide (contract-out
                                        ; table with alignments
            [table-with-alignment (-> string? (listof (listof any/c)) table?)]))
  (define (table-with-alignment alignment-str lines)
    (let* ((align-strings (regexp-split "\\." alignment-str))
           (numbered? 
            (char=? #\n (string-ref alignment-str (- (string-length alignment-str) 1))))
           (alignment-style-list 
            (map 
             (lambda (x) (style #f (parse-alignment-string x))) 
             (append '("l") align-strings '("r"))))
           (alignments (build-list (length lines) (lambda (m) alignment-style-list)))
           (style-list 
            (if numbered? 
                (list
                 (make-attributes (list (cons 'style "width:100%;")))
                 (table-columns 
                  (list 
                   (style #f (list
                              (column-attributes
                               (list
                                (cons 'style "width:45%;")))))
                   (style #f (list 
                              (column-attributes 
                               (list 
                                (cons 'span (number->string (- (length align-strings) 1)))))))
                   (style #f (list
                              (column-attributes
                               (list
                                (cons 'style "width:45%;")))))
                   (style #f (list
                              (column-attributes
                               (list
                                (cons 'style "width:5%")))))))
                 (table-cells alignments))
                (list 
                 (make-attributes (list (cons 'style "width:100%;")))
                 (table-columns
                  (list
                   (style #f (list
                              (column-attributes
                               (list
                                (cons 'style "width:45%;")))))
                   (style #f (list 
                              (column-attributes 
                               (list 
                                (cons 'span (number->string (length align-strings)))))))
                   (style #f (list
                              (column-attributes
                               (list
                                (cons 'style "width:45%;")))))))
                 (table-cells alignments))
                )))
      (table 
       (style #f style-list) 
       (if numbered? 
           (for/list ([xs lines])
             (for/list ([x (append '("") (drop-right xs 1) `("" ,(last xs)))])
               (if (block? x) x (para x))))
           (for/list ([xs lines])
             (for/list ([x (append '("") xs '(""))])
               (if (block? x) x (para x))))))
      ))
  (provide (contract-out
                                        ; table with alignments
            [dump-align (-> string? (listof (listof any/c)) paragraph?)]))
  (define (dump-align alignment-str lines)
    (let* ([align-strings (regexp-split "\\." alignment-str)]
           [numbered? 
            (char=? #\n (string-ref alignment-str (- (string-length alignment-str) 1)))]
           )
      (define (sanitize x) 
        (if (pre-content? x) 
            x 
            (let ([o (open-output-string)]) (write x o) (get-output-string o))))
            
      (para
       (literal "\\begin{align}")
       (linebreak)
       (let interleaveNL 
           ([rows (for/list ([ln lines])
                    (let ([formulas (if numbered? (drop-right ln 1) ln)])
                      (deep-literal
                       (map 
                        sanitize
                        `(,@(let interleave& ([xs formulas])
                              (if (cons? xs) 
                                  (if (cons? (cdr xs)) 
                                      (cons (car xs) (cons " & " (interleave& (cdr xs)))) 
                                      xs)
                                  '()))
                          ,(if (and numbered?
                                    (target-element? (last ln)))
                               (string-append "\\label{" (car (element-content (last ln))) "}") 
                               "")
                          )))))])
         (if (cons? rows) 
             (if (cons? (cdr rows)) 
                 (cons (car rows) `(,(linebreak) ,(literal "\\\\") ,(linebreak) ,@(interleaveNL (cdr rows))))
                 rows)
             '()))
       (linebreak)
       (literal "\\end{align}"))))
  (provide align)
  (define-syntax (align stx)
    (syntax-case stx ()
      [(_ alignment line ...)
       #`(table-with-alignment #,(symbol->string (syntax->datum #'alignment)) (list line ...))]))
  ;; ---------------------------------------------------------------------------------------------------
  (provide init-counter)
  (define-syntax (init-counter stx)
    (syntax-case stx ()
      ((_ counter-name)
       (let* ((name
               (symbol->string (syntax->datum #'counter-name)))
              (n 
               (string->unreadable-symbol 
                (string-append name "-n")))
              (hsh
               (string->unreadable-symbol 
                (string-append name "-hash"))))
         (datum->syntax 
          stx 
          `(begin 
             (define ,n 0)
             (define ,hsh (make-hash))
             (define (,(string->symbol (string-append name "-next")) label)
               (set! ,n (+ 1 ,n))
               (hash-set! ,hsh label ,n)
               (collect-element 
                (make-style #f '()) 
                (number->string ,n)
                (lambda (ci) void)
                ))
             (define (,(string->symbol (string-append name "-number")) label)
               (make-delayed-element
                (lambda (renderer pt ri) 
                  (if (hash-has-key? ,hsh label) 
                      (number->string (hash-ref ,hsh label))
                      (error (string-append "Counter " label " not found"))))
                (lambda () "mm")  ; sizer
                (lambda () "")    ; plain
                ))
             ))))))
;; ---------------------------------------------------------------------------------------------------
  (provide (contract-out
                                        ; This is a very universal style selector for element
   [bystro-elemstyle 
    (->* ((or/c #f string?)) () #:rest (listof any/c) style?)]))
  (define (bystro-elemstyle s . otherprops)
    (make-style 
     #f  
     (if s
         (cons 
                                        ; Used as a style property 
                                        ; to add arbitrary attributes to an HTML tag:
          (make-attributes (list (cons 'style s)))
          otherprops)
         otherprops
         )))
;; ---------------------------------------------------------------------------------------------------
  (provide (contract-out  [larger-2 (->* () () #:rest (listof pre-content?) element?)]))
  (define larger-2 (compose larger larger))
  (provide (contract-out  [larger-3 (->* () () #:rest (listof pre-content?) element?)]))
  (define larger-3 (compose larger larger larger))
  (provide (contract-out  [larger-4 (->* () () #:rest (listof pre-content?) element?)]))
  (define larger-4 (compose larger larger larger larger))
  (provide (contract-out  [smaller-2 (->* () () #:rest (listof pre-content?) element?)]))
  (define smaller-2 (compose smaller smaller))
  (provide (contract-out  [smaller-3 (->* () () #:rest (listof pre-content?) element?)]))
  (define smaller-3 (compose smaller smaller smaller))
  (provide (contract-out  [smaller-4 (->* () () #:rest (listof pre-content?) element?)]))
  (define smaller-4 (compose smaller smaller smaller smaller))
;; ---------------------------------------------------------------------------------------------------
  (provide bystro-rectangular-table?)
  (define (bystro-rectangular-table? a)
    (and (list? a) 
         (for/and ([y a]) (list? y))
         (let ([ly (length a)])
           (and ((length a) . > . 0)
                (let ([lx (length (car a))])
                  (and (lx . > . 0)
                       (for/and ([z (cdr a)]) 
                                (= (length z) lx))))))))
;; ---------------------------------------------------------------------------------------------------
  (provide (contract-out
                                        ; My table
            [tbl (->* (bystro-rectangular-table?) (#:orient (or/c 'hor 'vert)) table?)]))
  (define (tbl listofrows #:orient [dirn #f])
    (let* (
           [cell-style-suffix (if dirn
                                  (if (equal? dirn 'hor) 
                                      "-hor"
                                      "-vert")
                                  "")]
           [generic-cell-style
            (make-style (string-append "amktablecell" cell-style-suffix) '())]
           [topleft-cell-style
            (make-style (string-append "amktabletopleftcell" cell-style-suffix) '())]
           [left-cell-style
            (make-style (string-append "amktableleftcell" cell-style-suffix) '())]
           [top-cell-style
            (make-style (string-append "amktabletopcell" cell-style-suffix) '())]
           [style-def-first-row 
            (cons topleft-cell-style
                  (map (lambda (x) top-cell-style) (cdr (car listofrows)))
                  )]
           [style-def-generic-row
            (cons left-cell-style
                  (map (lambda (x) generic-cell-style) (cdr (car listofrows)))
                  )]
           [style-def
            (cons style-def-first-row
                  (map (lambda (x) style-def-generic-row) (cdr listofrows))
                  )]
           )
      (make-table (make-style #f (list (make-table-cells style-def)))
                  (map (lambda (x) 
                         (map (lambda (y) 
                                (if (block? y)
                                    y
                                    (if y (make-paragraph plain y) (make-paragraph plain ""))))
                              x) 
                         )
                       listofrows))))
  (provide (contract-out
            [bystro-table (->* (bystro-rectangular-table?)
                               (#:style-name string?
                                #:orient (or/c 'hor 'vert)
                                )
                               table?)]))
  (define (bystro-table listofrows
                        #:style-name [style-name "bystro-table"]
                        #:orient [dirn #f])
    (let* ([cell-style-suffix (if dirn (string-append "-" (symbol->string dirn)) "")]
           [generic-cell-style
            (make-style (string-append style-name "-cell" cell-style-suffix) '())]
           [topleft-cell-style
            (make-style (string-append style-name "-topleft-cell" cell-style-suffix) '())]
           [left-cell-style
            (make-style (string-append style-name "-left-cell" cell-style-suffix) '())]
           [top-cell-style
            (make-style (string-append style-name "-top-cell" cell-style-suffix) '())]
           [style-def-first-row 
            (cons topleft-cell-style
                  (map (lambda (x) top-cell-style) (cdr (car listofrows)))
                  )]
           [style-def-generic-row
            (cons left-cell-style
                  (map (lambda (x) generic-cell-style) (cdr (car listofrows)))
                  )]
           [style-def
            (cons style-def-first-row
                  (map (lambda (x) style-def-generic-row) (cdr listofrows))
                  )]
           )
      (make-table (make-style #f (list (make-table-cells style-def)))
                  (map (lambda (x) 
                         (map (lambda (y) 
                                (if (block? y)
                                    y
                                    (if y (make-paragraph plain y) (make-paragraph plain ""))))
                              x) 
                         )
                       listofrows)))
    )
;; ---------------------------------------------------------------------------------------------------
  (provide (contract-out  
                                        ; table filling 100% of width
   [longtbl (->* ((listof (listof block?))
                           #:styless (listof 
                                      (listof 
                                       (listof 
                                        (or/c 'left 'right 'center 'top 'baseline 'bottom 'vcenter)
                                        ))))
                          (#:width (integer-in 1 100))
                          nested-flow? )]))
  (define (longtbl bss #:styless ass #:width [w 100])
    (nested
     (make-table
      (make-style #f
                  (list
                   (make-attributes (list (cons 'style (string-append 
                                                        "width:" 
                                                        (number->string w) 
                                                        "%;"))))
                   (make-table-cells
                    (map 
                     (lambda (x) 
                       (map 
                        (lambda (y) (make-style #f y))
                        x))
                     ass))))
      bss)))
;; ---------------------------------------------------------------------------------------------------
  (provide (contract-out 
                                        ; My modified verbatim environment, to avoid line breaks
            [verb 
             (->* ((or/c string? #f)) 
                  (#:style style? 
                   #:indent exact-nonnegative-integer?) 
                  #:rest (listof string?) 
                  block?)]))
  (define (nolinebreaks p #:style [st #f])
    (make-table
     (if st st (make-style #f '()))
     (map (lambda (x)
            (list
             (make-paragraph (make-style #f (list 'div)) 
                             (paragraph-content (car x)))))
          (table-blockss p))))
  (define (verb #:style [st #f] #:indent [i 0] . x) 
    (nolinebreaks #:style st (apply verbatim #:indent i x)))
;; ---------------------------------------------------------------------------------------------------
  (provide (contract-out 
                                        ; colored text
   [clr (->* 
         ((or/c string? (list/c byte? byte? byte?))) 
         () 
         #:rest (listof pre-content?) 
         element?)])) 
  (define (clr clr-name . txt)
    (element (style #f (list (color-property clr-name))) 
      txt))
;; ---------------------------------------------------------------------------------------------------
  (define (bystro-is-scrbl? p #:exclude-same-name [x #t]) 
    (let-values ([(base name mustbedir) (split-path p)])
      (if (symbol? name) 
          #f
          (let* (
                 [ps (path->string name)]
                 [n (string-length ps)]
                 )
            (and 
             (equal? ".scrbl" (substring ps (max 0 (- n 6))))
             (not (and x (equal? (get-bystro-scrbl-filename) (path->string name)))))))))
;; ---------------------------------------------------------------------------------------------------
  (provide (contract-out 
            [bystro-dir-contains-scrbl? 
             (->* (path?) (#:exclude-same-name boolean?) boolean?)]))
  (define (bystro-dir-contains-scrbl? p #:exclude-same-name [x #f])
    (if (directory-exists? p)
        (pair? (find-files (curry bystro-is-scrbl? #:exclude-same-name x) p))
        #f))
;; ---------------------------------------------------------------------------------------------------
  (provide (contract-out
            [bystro-list-scrbls
             (->* (path-string?) (#:exclude-same-name boolean?) (listof path?))]))
  (define (bystro-list-scrbls p #:exclude-same-name [x #t])
    (let ([fs (directory-list p)])
      (filter 
       (λ (u) (bystro-is-scrbl? #:exclude-same-name x u))
       fs
      ))
    )
;; ---------------------------------------------------------------------------------------------------
  (provide (contract-out
            [bystro-list-scrbls-in-dir
             (->* (path-string?) (#:background-color (listof integer?))  element?)]))
  (define (bystro-list-scrbls-in-dir s #:background-color [clr '(251 206 177)])
    (apply 
     elem #:style (bystro-elemstyle #f (make-background-color-property clr))
     (flatten
      (map (lambda (u) 
             (let* ([x (path->string u)]
                    [n (string-length x)]
                    [bare (substring x 0 (- n 6))]
                    [h (string-append bare ".html")]
                    )
               (list 
                (hyperlink 
                 #:style (make-style 
                          "scrbllink" 
                          (list (make-css-addition (build-path 
                                                    css-dir
                                                    (string->path "misc.css")
                                                    ))))
                 (bystro-path-to-link (string-append s "/" h))  
                 bare) 
                " \n")))
           (bystro-list-scrbls s #:exclude-same-name #f)))))
;; ---------------------------------------------------------------------------------------------------
  (provide (contract-out [boldred (->* () #:rest (listof pre-content?) element?)]))
  (define (boldred . x) 
    (clr "red" (apply bold x)))
;; ---------------------------------------------------------------------------------------------------
                                        ; www section
                                        ; collection of functions useful for putting scribbles 
                                        ; as static content on a website

;; ---------------------------------------------------------------------------------------------------
  (provide (contract-out
                                        ; ribbon corresponding to some folder
            [bystro-ribbon-for-location (->* (path?) (#:exclude-same-name boolean?) block?)]))
  (define (bystro-ribbon-for-location p #:exclude-same-name [esn #f])
    (let* ([bystrotex.xml (build-path p "bystrotex.xml")]
           [bconf-xexpr (if (file-exists? bystrotex.xml)
                            (xml-file->bystroconf-xexpr bystrotex.xml)
                            #f)])
      (apply 
       para
       (for/list ([bc (if bconf-xexpr (se-path*/list '(scribblings) bconf-xexpr) '())]
                  #:when 
                  (and (cons? bc) 
                       (not (and esn (equal? (se-path* '(name) bc) (get-bystro-scrbl-name))))))
         (with-bystroconf 
          (get-bystroconf (get-bystro-scrbl-name))
          (Cname Cdest Cname.html Cname.scrbl Cformulas/ C.sqlite Carglist Cmultipage?)
          (with-bystroconf
           bc
           (name dest name.html name.scrbl formulas/ .sqlite arglist multipage?)
           (let* ([link-rel-to-rt
                   (if multipage?
                       (build-path p name "index.html")
                       (build-path p (if dest (string-append dest "/") 'same) (path->string name.html)))]
                  [link
                   (if (absolute-path? link-rel-to-rt)
                       link-rel-to-rt
                       (path->string (build-path (if (or Cmultipage? Cdest) "../" 'same) link-rel-to-rt)))])
             (elem 
              (if (file-exists? link-rel-to-rt)
                  (hyperlink 
                   #:style (make-style 
                            "scrbllink" 
                            (list (make-css-addition (build-path 
                                                      css-dir
                                                      (string->path "misc.css")
                                                      ))))
                   link
                   name)
                  name)
              (hspace 1)
              "\n"))))))))

;; ---------------------------------------------------------------------------------------------------
  (provide (contract-out 
                                        ; a nice ribbon with local scribblings
            [bystro-ribbon (->* () () block?)]))
  (define (bystro-ribbon) (bystro-ribbon-for-location (build-path 'same) #:exclude-same-name #t))

;; ---------------------------------------------------------------------------------------------------
  (provide (contract-out 
                                        ; dump the output of a shell command
            [bystro-shell-dump 
             (->* (string?) 
                  (#:stdin (or/c (and/c input-port? file-stream-port?) #f) 
                   #:style (or/c style? #f)
                   #:indent exact-nonnegative-integer?
                   )
                  #:rest (listof string?)
                  block?)]))
  (define (bystro-shell-dump #:stdin [stdin #f] #:style [style #f] #:indent [i 0] command . arguments)
    (define x (regexp-split #px"\\s" command))
    (define-values
      (process output inport errors)
      (apply 
       (curry subprocess #f stdin 'stdout (path->string (find-executable-path (car x)))) 
       (remove* (list "") (map string-trim (append (cdr x) arguments)))))
    (define output-string (port->string output))
    (close-input-port output)
    (nolinebreaks 
     #:style style 
     (verbatim 
      #:indent i 
      output-string
      )))

  (provide (contract-out 
            ; get the value of the command line argument with key --k
            [bystro-get-cl-argument (-> string? (or/c string? #f))]))
  (define (bystro-get-cl-argument k)
    (let v ([arglist (vector->list (current-command-line-arguments))])
      (if (cons? arglist)
          (if (equal? (car arglist) (string-append "--" k))
              (cadr arglist)
              (v (cdr arglist)))
          #f)))
  (provide (contract-out 
            ; get the value of the command line argument with key --k
            [bystro-cl-flag? (-> string? boolean?)]))
  (define (bystro-cl-flag? k)
    (let v ([arglist (vector->list (current-command-line-arguments))])
      (if (cons? arglist)
          (if (equal? (car arglist) (string-append "--" k))
              #t
              (v (cdr arglist)))
          #f)))
  (provide (contract-out
            ; get the path to the .scrbl file
            [bystro-source (-> element?)]))
  (define (bystro-source)
    (with-bystroconf 
      (get-bystroconf (get-bystro-scrbl-name))
      (Cname Cdest Cname.html Cname.scrbl Cformulas/ C.sqlite Carglist Cmultipage?)
      (if (or Cmultipage? Cdest)
          (hyperlink (path->string (build-path "../" Cname.scrbl)) (path->string Cname.scrbl))
          (hyperlink Cname.scrbl Cname.scrbl))))


)
