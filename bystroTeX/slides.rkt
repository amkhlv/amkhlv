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

  (require net/zmq)
  (require json)
  (require scribble/srcdoc (for-doc scribble/base scribble/manual))

  (provide (all-from-out db/base) (all-from-out db/sqlite3))

  ;; ------------------ start of compatibility block ---------------------

  (provide bystroserver)
  (define-struct/contract bystroserver 
    ([connection net:http-conn?] 
     [token string?]
     [user (or/c #f string?)]
     [host string?]
     [port number?]
     [path string?]
     [version (or/c #f string?)]
     )
    #:mutable)
  (provide (contract-out [struct bystro
                           ([formula-processor (or/c 'running-without-LaTeX-server bystroserver?)]
                            [formula-database-name path-string?]
                            [formula-dir-name path-string?]
                            [formula-size integer?]
                            [formula-bg-color (list/c (integer-in 0 255) (integer-in 0 255) (integer-in 0 255))]
                            [formula-fg-color (list/c (integer-in 0 255) (integer-in 0 255) (integer-in 0 255))]
                            [autoalign-adjust integer?]
                            [manual-base-alignment integer?])]))
  (struct bystro (
                  formula-processor
                  formula-database-name
                  formula-dir-name
                  formula-size
                  formula-bg-color
                  formula-fg-color
                  autoalign-adjust
                  manual-base-alignment
                  )
          #:mutable)
  (provide (proc-doc bystro-connect-to-server (->i ([xmlconf path?]) () [result 'running-without-LaTeX-server]) ()))
  (define (bystro-connect-to-server x) 'running-without-LaTeX-server)
  (define configuration (bystro 'running-without-LaTeX-server
                                "bystrotex.db"
                                "formulas"
                                25
                                (list 255 255 255)
                                (list 0 0 0)
                                1
                                (- 2)
                                ))

  ;; ---------------- end of compatibility block --------------------------------

  (define (get-zeromq-socket)
    (let* ([ctxt (context 1)]
           [sock (socket ctxt 'REQ)]
           [sock-path
            (format "ipc://~a/.local/run/bystrotex.ipc" (path->string (find-system-path 'home-dir)))]
           )
      (printf " -- connecting to ~a ~n" sock-path)
      (socket-connect! sock sock-path)
      (displayln " -- connected")
      sock))
  (define zeromq-socket (get-zeromq-socket))

  (provide (contract-out
            [bystro-close-connection (-> bystro? void?)]))
  (define (bystro-close-connection bconf)
    (when zeromq-socket (socket-close! zeromq-socket)))
  
  (define database-filename "bystrotex.db")
  (provide (proc-doc bystro-set-database-filename (->i ([filename string?]) () [result void?]) ()))
  (define (bystro-set-database-filename filename) (set! database-filename filename))
  
  (define formula-dir "formulas")
  (provide (proc-doc bystro-set-formula-dir (->i ([dirname string?]) () [result void?]) ()))
  (define (bystro-set-formula-dir dirname) (set! formula-dir dirname))


  (define equation-css-class "bystro-equation")
  (provide (contract-out
            (bystro-set-equation-css-class (-> string? void?))))
  (define (bystro-set-equation-css-class x) (set! equation-css-class x))
  (define formula-css-class "bystro-formula")
  (provide (contract-out
            (bystro-set-formula-css-class (-> string? void?))))
  (define (bystro-set-formula-css-class x) (set! formula-css-class x))
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
  (define (bystro-titlepage-init #:singlepage-mode [spmode #f])
    (if spmode
        (begin 
          (set-current-singlepage-mode! state #t)
          (bystro-css-element-from-files "misc.css" "slide.css")
          )
        (bystro-css-element-from-files "misc.css" "slide-title.css")))
;; ---------------------------------------------------------------------------------------------------
  (provide (contract-out
            ; Page
            [page (->* (content?) 
                       (#:tag (or/c symbol? string? #f) #:showtitle boolean?) 
                       pre-part?)]))
  (define (page stitle #:tag [tg #f] #:showtitle [sttl #f])
    (set-current-slide-number! state (+ 1 (current-slide-number state)))
    (set-current-slidename! state (if tg tg (regexp-replace #px"\\s" stitle "_")))
    (set-current-slide-part-number! state 0)
    (append
     `(,(part-start 0 #f (if tg `((part ,tg)) '()) (style #f to-hide) stitle))
     (if sttl `(,(element (make-style "pagetitle" '()) (list stitle))) '())
     `(,(bystro-css-element-from-files "misc.css" "slide.css")
       ,(collect-element 
         (make-style #f '()) 
         "" 
         (fn-to-collect-slide-link
          (current-slidename state)
          stitle 
          (current-slide-number state))))))

  (provide (contract-out
            ; Subsection in page
            [subpage (->*
                      (integer? content?) 
                      (#:tag (or/c symbol? string? #f)) 
                      pre-part?)]))
  (define (subpage depth stitle #:tag [tg #f])
    (set-current-slide-part-number! state (+ 1 (current-slide-part-number state)))
    (append
     `(,(part-start depth #f (if tg `((part ,tg)) '()) (style #f to-hide) stitle))
     `(,(element (make-style (string-append "pagetitle-" (number->string depth)) '()) (list stitle)))
     `(,(collect-element 
         (make-style #f '()) 
         "" 
         (fn-to-collect-subpage-link
          (if tg tg stitle)
          stitle 
          depth
          (current-slide-part-number state))))
     ))
      
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
          (decode `(,(title-decl #f tgs #f (style #f (cons 'toc-hidden to-hide)) "")
                    ,(bystro-css-element-from-files "misc.css" "slide.css")
                    ,more-content))
          (decode `(,(title-decl #f tgs #f (style #f stl) nm)
                    ,(bystro-css-element-from-files "misc.css" "slide.css")
                    ,@(current-content state))))))
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
  (define (fn-to-collect-subpage-link subpage-tag subpage-title subpage-depth n)
    (lambda (ci) 
      (collect-put! ci `(amkhlv-subpage ,subpage-tag ,subpage-depth ,n) subpage-title)))
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
    (if (current-singlepage-mode state)
        (decode (list
                 (title-decl 
                  #f 
                  (if tg (list (list 'part tg)) (list)) 
                  #f 
                  (style #f to-hide)
                  stitle)
                 (linebreak)
                 (bystro-css-element-from-files "misc.css" "slide.css")
                 (if sttl (element (make-style #f `(,(alt-tag "h2"))) `(,stitle ,(linebreak))) "")
                 (collect-element 
                  (make-style #f '()) 
                  "" 
                  (fn-to-collect-slide-link 
                   (current-slidename state) 
                   stitle 
                   (current-slide-number state)))
                 init-content))
        (begin
          (set-current-content!
           state
           (list
            (bystro-css-element-from-files "misc.css" "slide.css")
            (if sttl (element (make-style #f `(,(alt-tag "h2"))) `(,stitle ,(linebreak))) "")
            (collect-element 
             (make-style #f '()) 
             "" 
             (fn-to-collect-slide-link 
              (current-slidename state) 
              stitle 
              (current-slide-number state)))
            init-content))
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
      (and (not (for/or ([r sqlite-master_rows]) (equal? (vector-ref r 0) "formulas2")))
           (begin
             (query-exec mydb "CREATE TABLE formulas2 (tex, filename, valign, width, height)")
             (commit-transaction mydb))
           )
      (set-current-running-database! state mydb)
      mydb))
;; ---------------------------------------------------------------------------------------------------
  (provide (contract-out  
                                        ; enumerate a formula
   [number-for-formula (-> string? (or/c string? collect-element?))]))
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
         (error (string-append "ERROR: same label used twice -->" 
                               lbl 
                               "<-- refusing to proceed..."))
         ;(current-formula-ref-dict state) ;; do nothing if already registered such label
         (cons (cons lbl (current-formulanumber state)) (current-formula-ref-dict state))))
    (collect-element 
     (make-style #f '()) 
     (string-append "(" (number->string (current-formulanumber state)) ")")
     (fn-to-collect-formula-number lbl (current-formulanumber state))
     )
    )
;; ---------------------------------------------------------------------------------------------------
  (provide (contract-out 
                                        ; reference a formula
   [ref-formula (-> string? (or/c element? delayed-element?))]))
  (define (ref-formula lbl)
    (make-delayed-element
     (lambda (renderer pt ri) 
       (if (dict-has-key? (current-formula-ref-dict state) lbl)
           (number->string (dict-ref (current-formula-ref-dict state) lbl))
           (error (string-append "Formula reference -->" lbl "<-- is not found"))))
     (lambda () "100") ; TODO: what is this?
     (lambda () "")    ; TODO: what is this?
     ))
  ;; ---------------------------------------------------------------------------------------------------
  (define (get-svg-from-zeromq texstring filepath)
    (let* ([path
            (build-path (current-directory) filepath)]
           [j
             (make-hash
              (list (cons 'texstring texstring) (cons 'outpath (path->string path))))])
      (display j)
      (socket-send! zeromq-socket (jsexpr->bytes j))
      (define reply (socket-recv! zeromq-socket))
      ;(displayln "\n --- REPLY was: ")
      ;(displayln reply)
      (bytes->string/utf-8 reply)
      )
    )

  ;; ---------------------------------------------------------------------------------------------------
  (define (get-svg-from-server texstring size bg-color fg-color filepath)
                                        ; The procedure returns an integer representing the vertical offset.
    (let*-values
     ([(bserv) (values (bystro-formula-processor configuration))]
      [(u) (values
            (net:url
             "http"                          ;scheme
             (bystroserver-user bserv)      ;user
             (bystroserver-host bserv)      ;host 
             (bystroserver-port bserv)      ;port
             #t                              ;path-absolute?
             (list (net:path/param "svg" '()))   ;path
             (if (bystroserver-version bserv)
                 '()
                 (list 
                  (cons 'token (bystroserver-token bserv))
                  (cons 'latex texstring)
                  (cons 'size (number->string size))
                  (cons 'bg (rgb-list->string bg-color))
                  (cons 'fg (rgb-list->string fg-color)))) ;query (when version >= 2 we send in body)
             #f ;fragment
             ))]
      [(status headers inport)
       (net:http-conn-sendrecv!
        (bystroserver-connection bserv)
        (net:url->string u)
        #:method #"POST"
        #:headers '("BystroTeX:yes")
        #:data (if (bystroserver-version bserv) ; when version >= 2 we send everything in body
                   (jsexpr->string
                    (hash
                     'token
                     (bystroserver-token bserv)
                     'latex
                     texstring
                     'size
                     size
                     'bg
                     (rgb-list->string bg-color)
                     'fg
                     (rgb-list->string fg-color)))
                   #f)
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
           (with-output-to-file #:exists 'replace filepath (lambda () (display result)))
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
             (bystroserver-user bserv)      ;user
             (bystroserver-host bserv)      ;host 
             (bystroserver-port bserv)      ;port
             #t                              ;path-absolute?
             (list (net:path/param "bibtex" '()))   ;path
             (list 
              (cons 'token (bystroserver-token bserv))
              (cons 'k k))
             #f ;fragment
             ))]
      [(status headers inport)
       (net:http-conn-sendrecv!
        (bystroserver-connection bserv)
        (net:url->string u)
        #:method #"POST"
        #:headers '("BystroTeX:yes")
        )]
      [(error-type) (values
                     (for/first ([h headers] #:when (equal?
                                                     "BystroTeX-error:"
                                                     (car (string-split (bytes->string/utf-8 h)))))
                       (apply string-append (add-between (cdr (string-split (bytes->string/utf-8 h))) " "))))]                
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
         (let* ([bibxexpr
                 (with-handlers
                   ([exn:fail:read?
                     (λ (e)
                       (error (string-append "\n Citation not found: " k))
                       )
                     ])
                  (xml:xml->xexpr (xml:document-element (xml:read-xml inport)))
                  )
                 ]
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
                               ,texstring
                               )))
    (xml:write-xml/content
     (xml:xexpr->xml `(formula ((size ,(number->string size)) 
                                (bg ,(rgb-list->string bg-color)) 
                                (fg ,(rgb-list->string fg-color))
                                (filename ,filename)) 
                               ,(string-append preamble texstring))) 
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
            (display (string-append
                      found-error
                      "<--- ERROR processing LaTeX formula: \n"
                      texstring
                      "\nwith preamble: "
                      preamble))
            (error "*** please make corrections and run again ***")
            )
          ;; if no error, return the depth (as a string):
          (xml:se-path* '(depth) report-xexpr))))
  ;; ---------------------------------------------------------------------------------------------------

;; ---------------------------------------------------------------------------------------------------
  (provide (contract-out  
                                        ; corresponds to \equation in LaTeX
            [bystro-equation (->* ((listof string?) 
                          #:scale number?) 
                         (#:label (or/c string? #f)
                          #:css-class string?
                          )
                         table?)]))
  (define (bystro-equation 
           x 
           #:scale n 
           #:label [l #f] 
           #:css-class [css-class equation-css-class])
    (let* ([frml1 (keyword-apply bystro-formula '() '() x #:scale n #:css-class css-class #:align #f #:use-depth #t)]
           [frml frml1])
      (if l
          (table-with-alignment "c.n" (list (list frml (elemtag l (elemref l (number-for-formula l))))))
          (table-with-alignment "c.n" (list (list frml "" ))))))
;; ---------------------------------------------------------------------------------------------------
  (define (aligned-formula-image filepath valign width height css-class)
    ;(element 
     ;(bystro-elemstyle stl)
    ;(tg image #:attrs ([src (path->string (car (reverse (explode-path filepath))))])))
    ;(image #:style (style #f `(,stl)) filepath)
    (tg image
        #:attrs
        ([src (path->string (car (reverse (explode-path filepath))))]
         [class css-class]
         [style
          (format
           "~a width: ~a; height: ~a;"
           valign
           width
           height
           )]
         )))
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
                                  #:scale number?
                                  #:css-class string?
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
         #:scale [scale 1] 
         #:css-class [css-class formula-css-class]
         #:align [align #f] 
         #:use-depth [use-depth #f] 
         #:aa-adjust [aa-adj (bystro-autoalign-adjust configuration)] 
         . tex)
        (let* ([lookup (prepare 
                        mydb
                        "select filename,valign,width,height  from formulas2 where tex = ?")]
               [rows  (query-rows mydb (bind-prepared-statement
                                        lookup
                                        (list 
                                         (apply string-append tex))))]
               [row (if (cons? rows) (car rows) #f)]
               [totalnumber (query-value mydb "select count(*) from formulas2")]
               [rescale (λ (n x)
                          (if (eq? n 1)
                              x
                              (let ([sz (string->number (string-trim x "ex"))])
                                (string-append (number->string (* n sz)) "ex"))))]
               [realign (λ (n x)
                          (let* ([xs (regexp-match #rx"(.*vertical-align: *)(-*[\\.0-9]*)(.*)" x)])
                            (string-append
                             (cadr xs)
                             (number->string
                              (let ([orig (string->number (caddr xs))])
                                (if (orig . < . 0)
                                    (* (1 . - . (n . / . 10)) (string->number (caddr xs)))
                                    (* (1 . + . (n . / . 10)) (string->number (caddr xs))))))
                             (cadddr xs))))]
               )
          (if row
              (aligned-formula-image 
               (build-path formdir (string-append (vector-ref row 0) ".svg")) 
               (if align   (realign align (vector-ref row 1))   (vector-ref row 1))
               (rescale scale (vector-ref row 2))
               (rescale scale (vector-ref row 3))
               css-class
               )
              (let* 
                  ([formnum (totalnumber . + . 1)]
                   [filename (build-path formdir  (string-append (number->string formnum) ".svg"))]
                   [insert-stmt (prepare mydb "insert into formulas2 values (?,?,?,?,?)")]
                                        ;(tex, filename, valign, width, height)
                   [dims (get-svg-from-zeromq 
                          (apply string-append (cons preamble tex))
                          filename)]
                   [dims-json (with-input-from-string dims (λ () (read-json)))]
                   )
                (unless (string? dims) (error "ERROR: formula processor did not return style"))
                (displayln dims-json)
                (query
                 mydb
                 (bind-prepared-statement
                  insert-stmt 
                  `(,(apply string-append tex)
                    ,(number->string formnum) 
                    ,(hash-ref dims-json 'valign)
                    ,(hash-ref dims-json 'width)
                    ,(hash-ref dims-json 'height)
                    )))
                (commit-transaction mydb)
                (aligned-formula-image 
                 (build-path filename)
                 (if align (realign align (hash-ref dims-json 'valign)) (hash-ref dims-json 'valign))
                 (rescale scale (hash-ref dims-json 'width))
                 (rescale scale (hash-ref dims-json 'height))
                 css-class
                 )))))

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
             (list (element
                    (make-style
                     "bystro-toc-section"
                     `(,(target-url (string-append
                                     (regexp-replace* "[^-a-zA-Z0-9_=]" (car (cdr k)) "_")
                                     ".html"))))
                    ; Derivation of .html filename :  (define/override (derive-filename d ci ri depth) ...) in:
                    ; https://github.com/racket/scribble/blob/master/scribble-lib/scribble/html-render.rkt
                    (resolve-get pt ri k))
                   (linebreak)))))))))
;; ---------------------------------------------------------------------------------------------------
  (provide (contract-out  
                                        ; table of contents on the title-slide
   [bystro-local-toc (-> delayed-block?)]))
  (define (bystro-local-toc)
    (make-delayed-block 
     (lambda (renderer pt ri) 
       (let ([ks (resolve-get-keys pt ri (lambda (key)
                                           (eq? (car key) 'amkhlv-subpage)))])
         (apply 
          nested 
          (apply 
           append
           (for/list ([k (sort ks < #:key (lambda (k) (cadddr k)))])
             (list
              (hspace (* 4 (caddr k)))
              (element
               (make-style (string-append "local-toc-" (number->string (caddr k))) '())
               (seclink (car (cdr k)) (resolve-get pt ri k))
               )
              (linebreak)))))))))
;; ---------------------------------------------------------------------------------------------------
  (provide (contract-out
                                        ; padded on the top
            [v- (->* (integer?) () #:rest (listof pre-content?) (or/c (listof content?) table?))]))
  (define  (v- n . xs)
    (table 
     (style #f (list (table-cells (list  
                                   (list 
                                    (style 
                                     #f 
                                     (list 
                                      (attributes (list 
                                                   (cons 
                                                    'style 
                                                    (format
                                                     "padding:~apx;0px;0px;0px;"
                                                     (quotient
                                                      (* (bystro-formula-size configuration) n)
                                                      25))))))))
                                   (list (style #f '()))))))
     (list (list (para)) (list (if (block? xs) xs (apply para xs))))))
;; ---------------------------------------------------------------------------------------------------
  (provide (contract-out
                                        ; padded on the top
            [v+ (->* (integer?) () #:rest (listof pre-content?) (or/c (listof content?) table?))]))
  (define  (v+ n . xs)
    (table 
     (style #f (list (table-cells (list  
                                   (list (style #f '()))
                                   (list 
                                    (style 
                                     #f 
                                     (list 
                                      (attributes (list 
                                                   (cons 
                                                    'style 
                                                    (format
                                                     "padding:~apx;0px;0px;0px;"
                                                     (quotient
                                                      (* (bystro-formula-size configuration) n)
                                                      25))))))))
                                   ))))
     (list  (list (if (block? xs) xs (apply para xs))) (list (para)))))
;; ---------------------------------------------------------------------------------------------------
  (provide (contract-out
                                        ; padded on the left
            [h+ (->* (integer?) () #:rest (listof pre-content?) (or/c (listof content?) table?))]))
  (define  (h+ n . xs)
    (table 
     (style #f (list (table-cells (list  
                                   (list 
                                    (style 
                                     #f 
                                     (list 
                                      (attributes (list 
                                                   (cons 
                                                    'style 
                                                    (format
                                                     "padding:0px;~apx;0px;0px;"
                                                     (quotient
                                                      (* (bystro-formula-size configuration) n)
                                                      25))))))) 
                                    (style #f '()))))))
     (list (list (para) (if (block? xs) xs (apply para xs))))))
;; ---------------------------------------------------------------------------------------------------
  (provide (contract-out
                                        ; padded on the right
            [h- (->* (integer?) () #:rest (listof pre-content?) (or/c (listof content?) table?))]))
  (define  (h- n . xs)
    (table 
     (style
      #f
      `(,(table-cells `((,(style #f '())
                         ,(style #f `(,(attributes '(('style "padding:0ex;0.2ex;0ex;0ex;")))))
                         ,(style #f '())
                         )))))
     (list (list  (if (block? xs) xs (apply para xs)) (para)))))
)
