#!/usr/bin/env racket

#lang racket

(require racket/cmdline racket/string racket/list json xml xml/path bystroTeX/utils bystroTeX/xmlconf)

;; command line parsing
(define cleanup? (make-parameter #f))
(define show? (make-parameter #f))
(define verbose? (make-parameter #f))
(define locate-html? (make-parameter #f))
(define names (make-parameter '()))
(define items (make-parameter '()))

(names
 (command-line 
  #:once-each
  ["-c" ("cleanup") (cleanup? #t)]
  ["-s" ("show configuration") (show? #t)]
  ["-v" ("verbose") (verbose? #t)]
  ["-l" ("locate HTML file") (locate-html? #t)]
  #:args list-of-arguments
  list-of-arguments))


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

(unless (file-exists? "bystrotex.xml")
  (displayln "This is not a BystroTeX directory" (current-error-port))
  (exit))

(when (cleanup?) 
  (displayln "Cleanup!")
  (let* ([confs (se-path*/list '(scribblings) (bystroconf-xexpr))])
    (for ([c confs] #:when (cons? c))
      (with-bystroconf 
       c (name dest name.html name.scrbl formulas/ .sqlite arglist multipage?)
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
  (let* ([confs (se-path*/list '(scribblings) (bystroconf-xexpr))])
    (for ([c confs] #:when (cons? c))
      (when (verbose?) (displayln c))
      (with-bystroconf 
        c (name dest name.html name.scrbl formulas/ .sqlite arglist multipage?)
        (write-json
         (make-immutable-hasheq
          (append `(,(cons 'name name))
                  (if dest `(,(cons 'destination dest)) '())
                  (if multipage? `(,(cons 'multipage #t)) '())
                  (if (cons? arglist) `(,(cons 'arglist arglist)) '())
                  `(,(cons 'formulas (make-immutable-hasheq
                                      `(,(cons 'dir formulas/) ,(cons 'database .sqlite)))))
                  )))
        (displayln "")))))


(define (strip-ending x)
  (let* ([len (string-length x)]
         [lastchar (string-ref x (- len 1))]
         [.scrbl? (and (> len 6) (equal? (substring x (- len 6)) ".scrbl"))]
         [result (if (eqv? lastchar #\.)
                 (substring x 0 (- len 1))
                 (if .scrbl? ; strip the extension .scrbl if it is present
                     (substring x 0 (- len 6))
                     x))])
    result))

;; If names are absent, build ALL; otherwize, build those with matching names:
(items 
 (for/list ([c (se-path*/list '(scribblings) (bystroconf-xexpr))] 
            #:when 
            (and 
             (cons? c)
             (or 
              (not (or (show?) (cleanup?) (cons? (names))))
              (member 
               (with-bystroconf c (name dest name.html name.scrbl formulas/ .sqlite arglist multipage?) name) 
               (map strip-ending (names))))))
   c))

(for ([it (items)])
  (with-bystroconf 
    it 
    (name dest name.html name.scrbl formulas/ .sqlite arglist multipage?)
    (if (locate-html?)
        (if multipage?
            (if dest
                (displayln (path->string (build-path dest "index.html")))
                (displayln (path->string (build-path name "index.html"))))
            (if dest
                (displayln (path->string (build-path dest name.html)))
                (displayln name.html)))
        ;;otherwize BUILD:
        (if multipage?
            (begin
              (printf "Building ~a (multipage)\n" name)
              (run-and-show-results
               `("scribble"
                 ,@arglist
                 "++arg" "--bystro-name"
                 "++arg" ,name
                 "++arg" "--bystro-dest"
                 "++arg" ,(if dest dest name)
                 "++arg" "--htmls"
                 ,@(if dest `("--dest" ,dest) '())
                 "--htmls"
                 ,name.scrbl))
              ;(run-and-show-results `("ln" "-s" "-v" ,(path->string (build-path name "index.html")) ,name.html))
              )
            (begin
              (printf "Building ~a (singlepage)\n" name)
              (if dest
                  (begin
                    (run-and-show-results
                     `("scribble"
                       ,@arglist
                       "++arg" "--bystro-name"
                       "++arg" ,name
                       "++arg" "--bystro-dest"
                       "++arg" ,dest
                       "--dest" ,dest
                       ,name.scrbl))
                    ;(run-and-show-results `("ln" "-s" "-v" ,(path->string (build-path dest name.html)) "./"))
                    )
                  (run-and-show-results `("scribble" ,@arglist ,name.scrbl))))))))


