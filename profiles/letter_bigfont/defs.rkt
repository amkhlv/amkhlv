(module defs racket

  (require (for-syntax racket/base bystroTeX/slides_for-syntax racket/syntax))
  (require racket scribble/core scribble/base scribble/html-properties)
  (require bystroTeX/common bystroTeX/slides)

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
  (define (label s) (elemtag s (number-for-formula s)))  
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
                  [a (bystro-bg 176 224 230)]
                  [b (nested 
                      #:style (style "bystro-summary" 
                                     (list (make-attributes '((style . "background-color:PowderBlue;")))))
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

  )
