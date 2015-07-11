#!/usr/bin/env racket

#lang racket

(require racket/cmdline racket/string xml xml/path bystroTeX/utils)

;; command line parsing
(define cleanup? (make-parameter #f))
(define show? (make-parameter #f))
(define verbose? (make-parameter #f))
(define names (make-parameter '()))

(names
 (command-line 
  #:once-each
  ["-c" ("cleanup") (cleanup? #t)]
  ["-s" ("show configuration") (show? #t)]
  ["-v" ("verbose") (verbose? #t)]
  #:args list-of-arguments
  list-of-arguments))

(define  (file->xexpr x)
  (call-with-input-file x (λ (inport) (xml->xexpr (document-element (read-xml inport))))))

(define config (file->xexpr "bystrotex.xml"))

(define (delete-files-in-dir dir #:regex r #:depth [depth 1])
  (when (directory-exists? dir)
    (for/list ([f
                (find-files 
                 (λ (p)
                   (let ([ep (explode-path p)])
                     (and
                      ((length ep) . < . (+ 1 depth (length (explode-path dir))))
                      (regexp-match r (path->string (last ep))))))
                 dir)])
      (delete-file f))))

(define-syntax (with-conf stx)
  (syntax-case stx ()
    [(_ c '(name dest name.html name.scrbl formulas/ .sqlite multipage?) body ...)
     #'(let* ([name (let ([v (se-path* '(name) c)]) (if v (string-trim v) #f))]
              [dest (let ([v (se-path* '(dest) c)]) (if v (string-trim v) #f))]
              [formulas-dir (let ([v (se-path* '(formulas-dir) c)]) (if v (string-trim v) #f))]
              [sqlite-file  (let ([v (se-path* '(sqlite-file) c)]) (if v (string-trim v) #f))]
              [name.html (string->path (string-append name ".html"))]
              [name.scrbl (string->path (string-append name ".scrbl"))]
              [formulas/ (or formulas-dir name)]
              [multipage? (cons?    (filter   (λ  (x)  (equal? x '(multipage ())))   c))]
              [.sqlite  
               (or sqlite-file 
                   (if dest
                       (path->string (build-path dest "formulas.sqlite"))
                       (if multipage? (build-path name "formulas.sqlite")
                           (string-append name "_formulas.sqlite"))))])
         body ...)]))

(define (run-and-show-results cmdline)
  (with-external-command-as
   s
   #:cmdline cmdline
   (for ([l (in-lines s-stdout)]) (displayln l))
   (for ([l (in-lines s-stderr)]) (displayln l))
   (displayln " -----------------------------")))

(define (directory-exists-and-is-empty? d)
  (and 
   (directory-exists? d)
   (let ([dlist (directory-list d)])
     (if (cons? dlist)
       (begin
         (printf "Directory ---> ~a <--- exists but is not empty: " d)
         (displayln dlist)
         #f)
       #t))))

;; main:

(when (cleanup?) 
  (displayln "Cleanup!")
  (let* ([confs (se-path*/list '(scribblings) config)])
    (for ([c confs] #:when (cons? c))
      (with-conf 
       c '(name dest name.html name.scrbl formulas/ .sqlite multipage?)
       (when (file-exists? .sqlite) 
         (when verbose? (printf "Deleting the sqlite file ---> ~a\n" .sqlite))
         (delete-file .sqlite))
       (define ddir (or dest (if multipage? name #f)))
       (when ddir
         (for ([r '(#px"^scribble-common\\.js" #px"^\\d+\\.png" #px"^\\d+\\.svg" #px".css" #px".html")])
           (delete-files-in-dir (string->path ddir) #:regex r))
         (when (directory-exists-and-is-empty? (string->path ddir)) 
           (when verbose? (printf "Deleting the empty destination dir ---> ~a\n" ddir))
           (delete-directory (string->path ddir))))
       (define formdir formulas/)
       (for ([r '(#px"^\\d+\\.png" #px"^\\d+\\.svg")])
         (delete-files-in-dir (string->path formdir) #:regex r))
       (when (directory-exists-and-is-empty? (string->path formdir))
         (when verbose? (printf "Deleting the empty destination dir ---> ~a\n" formdir))
         (delete-directory (string->path formdir)))
       (when (or (file-exists? name.html) (link-exists? name.html))
         (when verbose? (printf "Deleting ---> ~a\n" name.html))
         (delete-file name.html))
       (when (directory-exists-and-is-empty? formulas/) 
         (when verbose? (printf "Deleting the empty formulas dir ---> ~a\n" formulas/))
         (delete-directory formulas/))
       (when (and multipage? (directory-exists-and-is-empty? name)) 
         (when verbose? (printf "Deleting the empty dir ---> ~a\n" name))
         (delete-directory name))))))

(when (show?)
  (displayln "Configuration")
  (displayln "=============")
  (let* ([confs (se-path*/list '(scribblings) config)])
    (for ([c confs] #:when (cons? c))
      (when (verbose?) (displayln c))
      (with-conf 
       c '(name dest name.html name.scrbl formulas/ .sqlite multipage?)
       (when multipage? (displayln "multipage:"))
       (printf "Name: ~a\n" name)
       (when dest (printf "Dest: ~a\n" dest))
       (printf "FDir: ~a\n" formulas/)
       (printf "SQL:  ~a\n" .sqlite)
       (displayln "")))))

;; If names are absent, build ALL:
(unless (or (show?) (cleanup?) (cons? (names)))
  (names 
   (let* ([confs (se-path*/list '(scribblings) config)])
     (for/list ([c confs] #:when (cons? c))
       (with-conf c '(name dest name.html name.scrbl formulas/ .sqlite multipage?) name)))))

(for ([nm (names)])
  (printf "Building ~a " nm)
  (let* ([nml (string-length nm)]
         [lastchar (string-ref nm (- nml 1))]
         [.scrbl   (equal? (substring nm (- nml 6)) ".scrbl")])
    (if (eqv? lastchar #\.) ; this is to facilitate TAB-completion
        (set! nm (substring nm 0 (- nml 1)))
        (when .scrbl ; strib the extension .scrbl if it is present
          (set! nm (substring nm 0 (- nml 6))))))
  (let* ([confs (se-path*/list '(scribblings) config)]
         [confcons (filter   
                    (λ  (x)  (let ([v (se-path* '(name) x)]) 
                               (and v (equal? (string-trim v) nm))))   
                    confs)]
         [conf (if (cons? confcons) 
                   (car confcons) 
                   (begin (error (string-append "ERROR: name --->" nm "<--- not found"))))])
    (with-conf 
     conf '(name dest name.html name.scrbl formulas/ .sqlite multipage?)
     (if multipage?
         (begin
           (displayln "(multipage)")
           (run-and-show-results `("scribble" "++arg" "--htmls" "--htmls" ,name.scrbl))
           (run-and-show-results `("ln" "-s" "-v" ,(path->string (build-path name "index.html")) ,name.html)))
         (begin
           (displayln "(singlepage)")
           (if dest
               (begin
                 (run-and-show-results `("scribble" "++arg" "--dest" "--dest" ,dest ,name.scrbl))
                 (run-and-show-results `("ln" "-s" "-v" ,(path->string (build-path dest name.html)) "./")))
               (run-and-show-results `("scribble" ,name.scrbl))))))))
