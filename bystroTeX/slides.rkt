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

(module slides racket
  (require scribble/core scribble/base scribble/html-properties scribble/decode scriblib/render-cond)
  (require "common.rkt")
  (require setup/dirs)
  (require scribble/decode)

  (require db/base db/sqlite3)
  (require racket/vector)
  (require racket/list)
  (require racket/dict)
  (require racket/system racket/file)

  (require racket/provide-syntax)
  
  (require (prefix-in xml: xml) (prefix-in xml: xml/path) (prefix-in xml: xml/xexpr))
  (require (prefix-in net: net/http-client))
  (require (prefix-in net: net/url))
  (require (prefix-in net: net/url-structs))

  (provide (all-from-out db/base) (all-from-out db/sqlite3))

;; ---------------------------------------------------------------------------------------------------
                                        ; Global variables
  (provide (struct-out bystro))
  (struct bystro (
                  formula-processor
                  formula-database-name
                  formula-dir-name
                  formula-size 
                  formula-bg-color
                  formula-fg-color
                  autoalign-adjust
                  manual-base-alignment
                  [extension #:auto]
                  )
          #:auto-value "svg"
          #:mutable)
  (provide (struct-out bystro-server))
  (struct bystro-server (
                         connection
                         token
                         user
                         host
                         port
                         path
                         )
          #:mutable)
  (provide (contract-out
                                        ; opens the server connection and returns the corresponding struct
            [bystro-connect-to-server (-> path? bystro-server?)]))
  (define (bystro-connect-to-server xmlconf-file)
    (let* ([server-conf (call-with-input-file xmlconf-file
                          (lambda (inport) (xml:xml->xexpr (xml:document-element (xml:read-xml inport)))))]
           [host (xml:se-path* '(host) server-conf)]
           [port (string->number (xml:se-path* '(port) server-conf))]
           [path (xml:se-path* '(path) server-conf)]
           [token (xml:se-path* '(token) server-conf)]
           )
      (bystro-server (net:http-conn-open host #:port port) token #f host port path)))
  (provide (contract-out
            [bystro-close-connection (-> bystro? void?)]))
  (define (bystro-close-connection bconf)
    (net:http-conn-close! (bystro-server-connection (bystro-formula-processor bconf))))
  (define configuration (bystro (find-executable-path "amkhlv-java-formula.sh")
                                "formulas.sqlite"
                                "formulas"
                                25
                                (list 255 255 255)
                                (list 0 0 0)
                                1
                                (- 2)
                                ))

  (provide (contract-out
                                        ; configures the bystro-conf 
            [configure-bystroTeX-using (-> bystro? void?)]))
  (define (configure-bystroTeX-using c)
    (set! configuration c))
  (define css-dir_slides (build-path 'same))
  (provide (contract-out
                                        ; Set the path to the folder containing the .css files
            [bystro-set-css-dir_slides (-> path? void?)]))
  (define (bystro-set-css-dir_slides x) (set! css-dir_slides x))
  (define preamble "")
  (provide (contract-out
                                        ; set the LaTeX preamble
            [use-LaTeX-preamble (->* () #:rest (listof string?) void?)]))
  (define (use-LaTeX-preamble . s)
    (set! preamble (string-append (apply string-append s) "\n")))
  (struct current (
                   slide-part-number
                   slide-number
                   slidename
                   content
                   formulanumber
                   formula-ref-dict
                   singlepage-mode
                   running-database
                   )
          #:mutable)
  (define state 
    (current 0 0 "SLIDE" '() 0 '() #f #f)) ; this is the global state
  (provide display-state)
  (define  (display-state s)
    (display (string-append "\n==========" s "=========\n"))
    (display (current-slidename state))
    (display (current-content state))
    (display (current-singlepage-mode state))
    (display (string-append "\n^^^^^^^^^^^^" s "^^^^^^^^\n"))
    )
;; ---------------------------------------------------------------------------------------------------
  (define to-hide (list 'non-toc 'no-toc 'unnumbered 'hidden 'hidden-number 'quiet))
;; ---------------------------------------------------------------------------------------------------
  (define (bystro-css-element-from-files . filenames)
    (make-element 
     (make-style #f 
                 (for/list ([fn filenames]) 
                  (make-css-addition (path->string (build-path css-dir_slides fn)))))
     '()))
;; ---------------------------------------------------------------------------------------------------
  (provide (contract-out 
                                        ; Titlepage initialization
   [bystro-titlepage-init (->* () (#:singlepage-mode boolean?) element?)]))
  (define (bystro-titlepage-init #:singlepage-mode [spm #f])
    (if spm
        (begin 
          (set-current-singlepage-mode! state #t)
          (bystro-css-element-from-files "misc.css" "slide.css")
          )
        (begin
          (bystro-css-element-from-files "misc.css" "slide-title.css")
          )
        )
    )
;; ---------------------------------------------------------------------------------------------------
  (provide (contract-out 
                                        ; Slide continuation after pause
   [after-pause (->* () 
                           (#:tag (or/c symbol? string? #f)) 
                           #:rest (listof (or/c part? pre-flow?) )
                           (or/c part? nested-flow?))]))  
  (define (after-pause #:tag [tg #f] . more-content)
    (set-current-slide-part-number! state (+ 1 (current-slide-part-number state)))
    (when (pair? more-content)
      (set-current-content! state (append (current-content state) (list more-content))))
    (let ([stl (if ((current-slide-part-number state) . < . 2) 
                   to-hide
                   (cons 'toc-hidden to-hide))]
          [nm  (if ((current-slide-part-number state) . < . 2)
                   (current-slidename state)
                   (if (pair? (current-slidename state))
                       (append 
                        (current-slidename state) 
                        (list " " (number->string (current-slide-part-number state))))
                       (string-append 
                        (current-slidename state) 
                        " " 
                        (number->string (current-slide-part-number state)))))]     
          [tgs (if tg (list (list 'part tg)) (list))])
      (if (current-singlepage-mode state)
          (begin
                                        ;(display-state "inside after-pause, singlepage")
            (decode (list (title-decl #f tgs #f (style #f (cons 'toc-hidden to-hide)) "") 
                          more-content)))
          (begin
                                        ;(display-state "inside after-pause, multipage")
            (decode 
             (cons (title-decl #f tgs #f (style #f stl) nm)
                   (current-content state))
             )))))
;; ---------------------------------------------------------------------------------------------------
  (provide (contract-out  
                                        ; removes the most recent after-pause
   [remove-slide (-> void?)]))
  (define (remove-slide)
    (if (pair? (current-content state)) 
        (set-current-content! state (reverse (cdr (reverse (current-content state)))))
        (error "nothing to remove !")))
;; ---------------------------------------------------------------------------------------------------
  (define (fn-to-collect-slide-link slide-shortname slide-title slide-num)
    (lambda (ci) 
      (collect-put! ci `(amkhlv-slide ,slide-shortname ,slide-num) slide-title)))
;; ---------------------------------------------------------------------------------------------------
  (provide (contract-out 
                                        ; slide
            [slide (->* (content?) 
                               (#:tag (or/c symbol? string? #f) #:showtitle boolean?) 
                               #:rest (listof (or/c pre-flow? part-start?) )
                               (or/c part? nested-flow?))]))  
  (define (slide stitle #:tag [tg #f] #:showtitle [sttl #f] . init-content)
    (set-current-slide-number! state (+ 1 (current-slide-number state)))
    (set-current-slide-part-number! state 0)
    (set-current-slidename! state (if tg 
                                      tg 
                                      (regexp-replace #px"\\s" stitle "_")))
    ;(display-state "in slide")
    (if (current-singlepage-mode state)
        (decode (list
                 (title-decl 
                  #f 
                  (if tg (list (list 'part tg)) (list)) 
                  #f 
                  (style #f to-hide)
                  stitle)
                 (linebreak)
                 (if sttl (para (clr "blue" (larger stitle)) (linebreak)) "")
                 (bystro-css-element-from-files "misc.css" "slide.css")
                 (collect-element 
                  (make-style #f '()) 
                  "" 
                  (fn-to-collect-slide-link 
                   (current-slidename state) 
                   stitle 
                   (current-slide-number state)))
                 init-content))
        (begin
          ;(display "multipage\n")
          (set-current-content!
           state
           (list 
            ;; (title-decl 
            ;;  #f 
            ;;  (if tg (list (list 'part tg)) (list))
            ;;  #f 
            ;;  (style #f to-hide)
            ;;  stitle)
            (if sttl (para (clr "blue" (larger stitle)) (linebreak)) "")
            (bystro-css-element-from-files "misc.css" "slide.css")
            (collect-element 
             (make-style #f '()) 
             "" 
             (fn-to-collect-slide-link 
              (current-slidename state) 
              stitle 
              (current-slide-number state)))
            init-content))
          ;(display-state "before calling afterpause")
          (after-pause  #:tag tg))))
;; ---------------------------------------------------------------------------------------------------
  (provide (contract-out  
                                        ; initialize formula collection dir and database
   [bystro-initialize-formula-collection 
    (-> bystro? connection?)]))
  (define (bystro-initialize-formula-collection bstr)
    (display "\n --- initializing formula collection in the directory: ")
    (display (bystro-formula-dir-name bstr))
    (display "\n --- using the sqlite file: ")
    (display (bystro-formula-database-name bstr))
    (unless (directory-exists? (string->path (bystro-formula-dir-name bstr)))
      (make-directory (string->path (bystro-formula-dir-name bstr))))
    (let* ([mydb (sqlite3-connect #:database (bystro-formula-database-name bstr) #:mode 'create)]
           [sqlite-master_rows (query-rows mydb "select name from SQLITE_MASTER")])
      (and (not (for/or ([r sqlite-master_rows]) (equal? (vector-ref r 0) "formulas")))
           (begin
             (query-exec mydb "CREATE TABLE formulas (tex, scale, bg, fg, filename, depth, tags)")
             (commit-transaction mydb))
           )
      (set-current-running-database! state mydb)
      mydb))
;; ---------------------------------------------------------------------------------------------------
  (provide (contract-out  
                                        ; enumerate a formula
   [number-for-formula (-> string? collect-element?)]))
  (define (fn-to-collect-formula-number lbl n)
    (lambda (ci) void)
    ;; (lambda (ci) 
    ;;   (collect-put! ci `(amkhlv-formula-number ,lbl ,n) #f))
    )
  (define (number-for-formula lbl)
    (set-current-formulanumber! state (+ 1 (current-formulanumber state)))
    (set-current-formula-ref-dict! 
     state
     (if (dict-has-key? (current-formula-ref-dict state) lbl) 
         (error (string-append "ERROR: same label used twice:\n" lbl "\n-- refusing to proceed..."))
         ;(current-formula-ref-dict state) ;; do nothing if already registered such label
         (cons (cons lbl (current-formulanumber state)) (current-formula-ref-dict state))))
    (collect-element 
     (make-style #f '()) 
     (string-append "(" (number->string (current-formulanumber state)) ")")
     (fn-to-collect-formula-number lbl (current-formulanumber state))
    ))
;; ---------------------------------------------------------------------------------------------------
  (provide (contract-out 
                                        ; reference a formula
   [ref-formula (-> string? delayed-element?)]))
  (define (ref-formula lbl)
        (make-delayed-element
         (lambda (renderer pt ri) 
           (if (dict-has-key? (current-formula-ref-dict state) lbl)
               (number->string (dict-ref (current-formula-ref-dict state) lbl))
               (error (string-append "Formula reference" lbl " is not found"))))
         (lambda () "100") ; TODO: what is this?
         (lambda () "")    ; TODO: what is this?
         ))
  ;; ---------------------------------------------------------------------------------------------------
  (define (get-svg-from-server texstring size bg-color fg-color filename)
                                        ; The procedure returns an integer representing the vertical offset.
    (let*-values
     ([(bserv) (values (bystro-formula-processor configuration))]
      [(u) (values
            (net:url
             "http"                          ;scheme
             (bystro-server-user bserv)      ;user
             (bystro-server-host bserv)      ;host 
             (bystro-server-port bserv)      ;port
             #t                              ;path-absolute?
             (list (net:path/param "svg" '()))   ;path
             (list 
              (cons 'token (bystro-server-token bserv))
              (cons 'latex texstring)
              (cons 'size (number->string size))
              (cons 'bg (rgb-list->string bg-color))
              (cons 'fg (rgb-list->string fg-color))) ;query
             #f ;fragment
             ))]
      [(status headers inport)
       (net:http-conn-sendrecv!
        (bystro-server-connection bserv)
        (net:url->string u)
        #:method #"POST"
        #:headers '("BystroTeX:yes")
        )]
      [(result) (values (port->string inport))]
      [(error-type) (values
                     (for/first ([h headers] #:when (equal?
                                                     "BystroTeX-error:"
                                                     (car (string-split (bytes->string/utf-8 h)))))
                       (cadr (string-split (bytes->string/utf-8 h)))))]                
      )
     (close-input-port inport)
     (if error-type
         (begin 
           (display (string-append "\n\n --- ERROR of the type: <<"
                                   error-type
                                   ">>, while processing:\n"
                                   texstring
                                   "\n\n --- The error message was:\n"
                                   result))
           (error "*** please make corrections and run again ***")
           )
         (let ([depth-string (for/first ([h headers]
                                         #:when (equal?
                                                 "BystroTeX-depth:"
                                                 (car (string-split (bytes->string/utf-8 h)))))
                               (cadr (string-split (bytes->string/utf-8 h))))])
           (with-output-to-file #:exists 'replace filename (lambda () (display result)))
           depth-string))))
  ;; ---------------------------------------------------------------------------------------------------
  (provide (contract-out  
                                        ; enumerate a formula
   [get-bib-from-server (-> string? hash?)]))
  (define (get-bib-from-server k)
    (let*-values
     ([(bserv) (values (bystro-formula-processor configuration))]
      [(u) (values
            (net:url
             "http"                          ;scheme
             (bystro-server-user bserv)      ;user
             (bystro-server-host bserv)      ;host 
             (bystro-server-port bserv)      ;port
             #t                              ;path-absolute?
             (list (net:path/param "bibtex" '()))   ;path
             (list 
              (cons 'token (bystro-server-token bserv))
              (cons 'k k))
             #f ;fragment
             ))]
      [(status headers inport)
       (net:http-conn-sendrecv!
        (bystro-server-connection bserv)
        (net:url->string u)
        #:method #"POST"
        #:headers '("BystroTeX:yes")
        )]
      [(error-type) (values
                     (for/first ([h headers] #:when (equal?
                                                     "BystroTeX-error:"
                                                     (car (string-split (bytes->string/utf-8 h)))))
                       (cadr (string-split (bytes->string/utf-8 h)))))]                
      )
     (if error-type
         (begin 
           (display (string-append "\n\n --- ERROR of the type: <<"
                                   error-type
                                   ">>, while processing the BibTeX entry:\n"
                                   k
                                   "\n\n --- The error message was:\n"
                                   (port->string inport)))
           (close-input-port inport)
           (error "*** please make corrections and run again ***")
           )
         (let* ([bibxexpr (xml:xml->xexpr (xml:document-element (xml:read-xml inport)))]
                [xs (xml:se-path*/list '(bibentry) bibxexpr)]
                )
           (close-input-port inport)
           (make-hash 
            (for/list ([x xs] #:when (cons? x)) 
              (cons (string-downcase (xml:se-path* '(v #:key) x)) (xml:se-path* '(v) x)))))
         )))
  ;; ---------------------------------------------------------------------------------------------------
  (define (bystro-command-to-typeset-formula shell-command-path texstring size bg-color fg-color filename)
                                        ; The procedure returns an integer representing the vertical offset.
    (define-values (pr outport inport errport) 
      (subprocess #f #f #f shell-command-path))
    (display "\n")
    (xml:write-xml/content
     (xml:xexpr->xml `(formula ((size ,(number->string size)) 
                                (bg ,(rgb-list->string bg-color)) 
                                (fg ,(rgb-list->string fg-color))
                                (filename ,filename)) 
                               ,(substring texstring (string-length preamble)))))
    (xml:write-xml/content
     (xml:xexpr->xml `(formula ((size ,(number->string size)) 
                                (bg ,(rgb-list->string bg-color)) 
                                (fg ,(rgb-list->string fg-color))
                                (filename ,filename)) 
                               ,texstring)) 
     inport)
    (close-output-port inport)
    (let* (
           [report-xml (xml:read-xml outport)]
           [report-xexpr (xml:xml->xexpr (xml:document-element report-xml))]
           [found-error (xml:se-path* '(error) report-xexpr)]
           )
      (close-input-port outport)
      (close-input-port errport)
      (if found-error
          (begin 
            (display (string-append found-error "<--- ERROR processing LaTeX formula: \n" texstring))
            (error "*** please make corrections and run again ***")
            )
          ;; if no error, return the depth (as a string):
          (xml:se-path* '(depth) report-xexpr))))
  ;; ---------------------------------------------------------------------------------------------------

;; ---------------------------------------------------------------------------------------------------
  (provide (contract-out  
                                        ; corresponds to \equation in LaTeX
            [bystro-equation (->* ((listof string?) 
                          #:size natural-number/c) 
                         (#:label (or/c string? #f)
                          #:bg-color (listof natural-number/c)
                          #:fg-color (listof natural-number/c)
                          )
                         table?)]))
  (define (bystro-equation 
           x 
           #:size n 
           #:label [l #f] 
           #:bg-color [bgcol (bystro-formula-bg-color configuration)] 
           #:fg-color [fgcol (bystro-formula-fg-color configuration)])
    (if l
        (table-with-alignment 
         "c.n" 
         (list (list 
                (keyword-apply bystro-formula '() '() x #:size n #:bg-color bgcol #:fg-color fgcol #:align #f #:use-depth #t)
                (elemtag l (number-for-formula l)))))
        (table-with-alignment
         "c.n" 
         (list (list 
                (keyword-apply bystro-formula '() '() x #:size n #:bg-color bgcol #:fg-color fgcol #:align #f #:use-depth #t)
                "" )))))
;; ---------------------------------------------------------------------------------------------------
  (define (aligned-formula-image manual-adj use-depth depth aa-adj filepath sz)
    (element 
        (bystro-elemstyle 
         (cond
          [manual-adj (string-append 
                       "display:inline;white-space:nowrap;vertical-align:-" 
                       (number->string (+ aa-adj depth (- (round (/ (* manual-adj sz) 18))))) 
                       "px")]
          [use-depth (string-append 
                      "display:inline;white-space:nowrap;vertical-align:-" 
                      (number->string (+ aa-adj depth)) 
                      "px" )]
          [else "display:inline;white-space:nowrap;vertical-align:middle"]))
      (image  filepath)))
;; ---------------------------------------------------------------------------------------------------
  (define (rgb-list->string x) 
    (string-append
     (number->string (car x)) 
     ":"
     (number->string (cadr x))
     ":"
     (number->string (caddr x))))
  (provide (contract-out  
                                        ; inline formula
            [bystro-formula (->* () 
                                 (#:shell-command path?
                                  #:database connection? 
                                  #:formulas-in-dir string?
                                  #:size natural-number/c 
                                  #:bg-color (listof natural-number/c)
                                  #:fg-color (listof natural-number/c) 
                                  #:align (or/c (integer-in (- 99) 99) #f) 
                                  #:use-depth boolean? 
                                  #:aa-adjust (integer-in (- 99) 99)
                                  ) 
                                 #:rest (listof string?) 
                                 element? )]))
  (define (bystro-formula 
           #:shell-command [shell-command-path (bystro-formula-processor configuration)]
           #:database [mydb (current-running-database state)]
           #:formulas-in-dir [formdir (bystro-formula-dir-name configuration)]
           #:size [bsz (bystro-formula-size configuration)] 
           #:bg-color [bg-color (bystro-formula-bg-color configuration)]
           #:fg-color [fg-color (bystro-formula-fg-color configuration)]
           #:align [align #f] 
           #:use-depth [use-depth #f] 
           #:aa-adjust [aa-adj (bystro-autoalign-adjust configuration)] 
           . tex)
    (let* ([lookup (prepare 
                    mydb
                    "select filename,depth  from formulas where scale = ? and tex = ? and bg = ? and fg = ?")]
           [rows  (query-rows mydb (bind-prepared-statement
                                    lookup
                                    (list 
                                     bsz 
                                     (apply string-append (cons preamble tex))
                                     (rgb-list->string bg-color) 
                                     (rgb-list->string fg-color))))]
           [row (if (cons? rows) (car rows) #f)]
           [totalnumber (query-value mydb "select count(*) from formulas")]
           )
      (if row
          (aligned-formula-image 
           align 
           use-depth 
           (string->number (vector-ref row 1)) 
           aa-adj 
           (build-path formdir (string-append (vector-ref row 0) "." (bystro-extension configuration))) 
           bsz)
          (let* 
              ([formnum (totalnumber . + . 1)]
               [filename (string-append formdir "/" (number->string formnum) "." (bystro-extension configuration))]
               [insert-stmt (prepare mydb "insert into formulas values (?,?,?,?,?,?,?)")]
                                        ;(tex, scale, bg, fg, filename, depth, tags)
               [procedure-to-typeset-formula
                (if (bystro-server? (bystro-formula-processor configuration))
                    get-svg-from-server
                    (curry bystro-command-to-typeset-formula (bystro-formula-processor configuration)))]
               [dpth-str (procedure-to-typeset-formula
                          (apply string-append (cons preamble tex))
                          bsz 
                          bg-color
                          fg-color
                          filename)])
            (unless (string? dpth-str) (error "ERROR: procedure to typeset formulas did not return the depth string"))
            (query
             mydb
             (bind-prepared-statement
              insert-stmt 
              (list (apply string-append (cons preamble tex))
                    bsz 
                    (rgb-list->string bg-color) 
                    (rgb-list->string fg-color) 
                    (number->string formnum) 
                    dpth-str 
                    "")))
            (commit-transaction mydb)
            (aligned-formula-image 
             align 
             use-depth 
             (string->number dpth-str) 
             aa-adj 
             (build-path filename) 
             bsz)))))
;; ---------------------------------------------------------------------------------------------------
  (provide (contract-out 
                                        ; change the background color for the formulas
            [bystro-bg (-> natural-number/c natural-number/c natural-number/c void?)]))
  (define (bystro-bg r g b)
    (set-bystro-formula-bg-color! configuration (list r g b)))
;; ---------------------------------------------------------------------------------------------------
  (provide (contract-out 
                                        ; change the background color for the formulas
            [bystro-fg (-> natural-number/c natural-number/c natural-number/c void?)]))
  (define (bystro-fg r g b)
    (set-bystro-formula-fg-color! configuration (list r g b)))
;; ---------------------------------------------------------------------------------------------------
  (provide (contract-out  
                                        ; table of contents on the title-slide
   [bystro-toc (-> delayed-block?)]))
  (define (bystro-toc)
    (make-delayed-block 
     (lambda (renderer pt ri) 
       (let ([ks (resolve-get-keys pt ri (lambda (key)
                                           (eq? (car key) 'amkhlv-slide)))])
         (apply 
          nested 
          (apply 
           append
           (for/list ([k (sort ks < #:key (lambda (k) (caddr k)))])
             (list (seclink (car (cdr k)) (resolve-get pt ri k)) (linebreak)))))))))
;; ---------------------------------------------------------------------------------------------------
)
