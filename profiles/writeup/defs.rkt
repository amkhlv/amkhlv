(module defs racket

  (require (for-syntax racket/base bystroTeX/slides_for-syntax racket/syntax))
  (require racket scribble/core scribble/base scribble/html-properties scribble/decode)
  (require bystroTeX/common bystroTeX/slides truques/truques)

  ;; Here the basic syntax can be adjusted:
  (provide bystro-def-formula)
  (define-syntax (bystro-def-formula stx)
    (bystro-formula-syntax 
     #:autoalign-formula-prefix "f"
     #:manual-formula-prefix    "f"
     #:display-math-prefix      "e"
     #:size-change-notation     "fsize"
     #:size-increase-notation   "fsize+"
     #:size-restore-notation    "fsize="
     #:max-size-increase        8
     #:max-size-decrease        5
     #:max-vert-adjust          8
     stx))

  ;; Here we define new functions:
  (provide label)
  (define (label s) (elemtag s (elemref s (number-for-formula s))))  
  (provide ref)
  (define (ref s) (elemref s (ref-formula s)))
  (provide red)
  (define (red . x) (apply clr (cons "red" x)))
  (provide green)
  (define (green . x) (apply clr (cons "green" x)))
  (provide greenbox-style)
  (define (greenbox-style more) 
    (bystro-elemstyle (string-append "border-style:solid;border-color:#00aa00;" more)))
  (provide redbox-style)
  (define (redbox-style more)   
    (bystro-elemstyle (string-append "border-style:solid;border-color:#aa0000;" more)))
  (provide greenbox)
  (define (greenbox more . x) (elem #:style (greenbox-style more) x))
  (provide redbox)
  (define (redbox   more . x) (elem #:style (redbox-style more) x))
  (provide greenbox-wide)
  (define (greenbox-wide more . x) (nested #:style (greenbox-style more) x))
  (provide redbox-wide)
  (define (redbox-wide   more . x) (nested #:style (redbox-style   more) x))
  (provide hrule)
  (define (hrule) (element (make-style #f (list (alt-tag "hr"))) ""))
  (provide leftbar)
  (define (leftbar . x) 
    (para 
     #:style (bystro-elemstyle 
              "border-left-style:solid;border-color:green;padding-left:12px;") 
     x))
  (require racket/dict)
  (init-counter exercise)
  (init-counter theorem)
  (provide ex-num ex-ref th-num th-ref defn-num defn-ref)
  (define (ex-num label)
    (elemtag label (exercise-next label)))
  (define (ex-ref label)
    (elemref label (list (exercise-number label))))
  (define (th-num label)
    (elemtag label (theorem-next label)))
  (define (th-ref label)
    (elemref label (list (theorem-number label))))
  (define (defn-num label)
    (elemtag label (exercise-next label)))
  (define (defn-ref label)
    (elemref label (list (exercise-number label))))
  (provide noindent)
  (define noindent "")

  (provide comment)
  (define-syntax (comment stx)
    (syntax-case stx ()
      [(_ x ...)
       (with-syntax ([bc (format-id stx "bystro-conf")])
         #'(let* ([oldbg (bystro-formula-bg-color bc)]
                  [a (bystro-bg 240 240 255)]
                  [s1 (set-bystro-formula-size! bc (- (bystro-formula-size bc) 3))]
                  [b (nested 
                      #:style (style "comment" 
                                     (list (make-attributes '((style . "background-color:rgb(240,240,255);")))))
                      x ...)]
                  [s2 (set-bystro-formula-size! bc (+ (bystro-formula-size bc) 3))]
                  [c (apply bystro-bg oldbg)])
             b))]))
  (provide short-intro)
  (define-syntax (short-intro stx)
    (syntax-case stx ()
      [(_ x ...)
       (with-syntax ([bc (format-id stx "bystro-conf")])
         #'(let* ([oldbg (bystro-formula-bg-color bc)]
                  [a (bystro-bg 240 255 240)]
                  [b (nested 
                      #:style (style "greenbox" 
                                     (list (make-attributes '((style . "background-color:rgb(240,255,240);")))))
                      x ...)]
                  [c (apply bystro-bg oldbg)])
             b))]))
  (provide summary)
  (define-syntax (summary stx)
    (syntax-case stx ()
      [(_ x ...)
       (with-syntax ([bc (format-id stx "bystro-conf")])
         #'(let* ([oldbg (bystro-formula-bg-color bc)]
                  [a (bystro-bg 255 255 255)]
                  [b (nested
                      #:style (style "bystro-summary" '())
                      x ...)]
                  [c (apply bystro-bg oldbg)])
             b))]))
  (provide quotation)
  (define-syntax (quotation stx)
    (syntax-case stx ()
      [(_ x ...)
       (with-syntax ([bc (format-id stx "bystro-conf")])
         #'(let* ([oldbg (bystro-formula-bg-color bc)]
                  [a (bystro-bg 253 237 236)]
                  [b (nested 
                      #:style (style "bystro-quotation" 
                                     (list (make-attributes '((style . "background-color:rgb(253,237,236);")))))
                      x ...)]
                  [c (apply bystro-bg oldbg)])
             b))]))
  (provide indent)
  (define (indent . xs) (nested #:style (style "bystro-indent-1" '()) xs))
  (provide indent--->)
  (define (indent---> . xs) (nested #:style (style "bystro-indent-2" '()) xs))
  (provide appendix)
  (define appendix (elem #:style (style "bystro-end-of-main-text" '()) '()))
  (provide bystro-abstract)
  (define (bystro-abstract . xs)
    (nested
     #:style (style "bystro-abstract" '())
     xs))
  (provide bystro-authors)
  (define (bystro-authors . xs)
    (nested
     #:style (style "bystro-authors" '())
     xs))
  (provide bystro-margin-note)
  (define bystro-margin-note margin-note*)
  (provide bystro-scrbl-only)
  (define-syntax (bystro-scrbl-only stx) (syntax-case stx () [(_ x ...) #'(begin x ...)]))
  (provide bystro-latex-only)
  (define (bystro-latex-only . xs) (elem '()))
  (provide marg)
  (define
    (marg #:scale s #:dir d #:filter f #:build-for-local [build-for-local #t])
    (bystro-margin-note
     (if build-for-local
         (autolist-svgs
          #:scale s
          #:dir d
          #:ncols 1
          #:showdir #f
          #:filter f
          )
         (autolist
          #:exts '(svg)
          #:dir d
          #:filter f
          #:output (lambda (x)
                     `(,(hyperlink
                         (path->string x)
                         (image
                          #:scale s
                          (find-relative-path
                           (current-directory)
                           (path->complete-path (build-path d x))))
                         )))))))

  )
