#!/usr/bin/env racket

#lang at-exp racket

(require parser-tools/lex
         (prefix-in re- parser-tools/lex-sre)
         parser-tools/yacc
         racket/string
         racket/list
         )

(define-syntax (lexabbr syn)
  (define x (cadr (syntax->list syn)))
  (define y (syntax->datum (caddr (syntax->list syn))))
  (syntax-case syn ()
    ((lexabbr name s)
     (datum->syntax #'s (list 'define-lex-abbrev x (cons 're-: (string->list y)))))))

@lexabbr[beq]{\begin{equation}}
@lexabbr[eeq]{\end{equation}}
@lexabbr[baln]{\begin{align}}
@lexabbr[ealn]{\end{align}}
@lexabbr[barr]{\begin{array}}
@lexabbr[earr]{\end{array}}
@lexabbr[lbl]{\label}
@lexabbr[nonum]{\nonumber}

;; ---------------------------------------------------------------------------------------------------

(define-empty-tokens 
  delims 
  (DOLLAR BEGEQ ENDEQ BALIGN EALIGN LBRACE RBRACE LABEL NONUM BARRAY EARRAY EOF))
(define-tokens chrs (CHR))

(define-lex-abbrevs
;  (control-chars (union #\\ #\$))
  (control-chars (union #\$))
  )

(define latex-lexer
  (lexer
   ("$" (token-DOLLAR))
   ((char-complement control-chars) (token-CHR lexeme))
   (whitespace (token-CHR " "))
   (beq (token-BEGEQ))
   (eeq (token-ENDEQ))
   (baln (token-BALIGN))
   (ealn (token-EALIGN))
   (barr (token-BARRAY))
   (earr (token-EARRAY))
   (nonum (token-NONUM))
   ((eof) (token-EOF))
   ))

(define-struct inline-formula (txt))
(define-struct eq (txt))
(define-struct aln (txt))
(define-struct label (txt))

(define latex-parser
  (parser
   (start text)
   (end EOF)
   (error (lambda (tok-ok? tok-name tok-value) (display tok-name)))
   (tokens delims chrs)
   (grammar
    (text 
     ((fml text) (cons $1 $2))
     ((align text) (cons $1 $2))
     ((inl text) (cons $1 $2))
     ((CHR text) (cons $1 $2))
     ((CHR) (list $1))
     ((fml) (list $1))
     ((align) (list $1))
     ((inl) (list $1)))
    (ws  ; this gives a string
     ((NONUM ws) $2)
     ((NONUM) "")
     ((BARRAY ws EARRAY) (string-append "\\begin{array}" $2 "\\end{array}"))
     ((BARRAY ws EARRAY ws) (string-append "\\begin{array}" $2 "\\end{array}" $4))
     ((CHR ws) (string-append $1 $2))
     ((CHR) $1)
     )
    (fml ; this gives an eq struct
     ((BEGEQ ws ENDEQ) (make-eq $2)))
    (align ; this gives an aln struct
     ((BALIGN ws EALIGN) (make-aln $2)))
    (inl ; this gives an inline-formula struct
     ((DOLLAR ws DOLLAR) (make-inline-formula $2)))
    )))

(define lbl-lexer
  (lexer
   ((char-complement (union #\{ #\})) (token-CHR lexeme))
   ("{" (token-LBRACE))
   ("}" (token-RBRACE))
   (whitespace (token-CHR " "))
   (lbl (token-LABEL))
   ((eof) (token-EOF))
   ))

(define-struct text-with-lbl (text label))
(define (prepend-to-text-with-lbl x twl)
  (struct-copy text-with-lbl twl [text (string-append x (text-with-lbl-text twl))]))

(define lbl-parser
  (parser
   (start text)
   (end EOF)
   (error (lambda (tok-ok? tok-name tok-value) (display tok-name)))
   (tokens delims chrs)
   (grammar
    (text ; this gives a struct text-with-lbl
     ((CHR text) (prepend-to-text-with-lbl $1 $2))
     ((LBRACE text) (prepend-to-text-with-lbl "{" $2))
     ((RBRACE text) (prepend-to-text-with-lbl "}" $2))
     ((RBRACE) (make-text-with-lbl "}" #f))
     ((CHR) (make-text-with-lbl $1 #f))
     ((LABEL LBRACE ws RBRACE text) (struct-copy text-with-lbl $5 [label $3]))
     ((LABEL LBRACE ws RBRACE) (make-text-with-lbl "" $3))
     )
    (ws ; this gives a string
     ((CHR ws) (string-append $1 $2))
     ((CHR) $1)
     )
    )))

(define (labelled u) ; the result is the struct text-with-lbl
  (let ((input (open-input-string u)))
    (lbl-parser (lambda () (lbl-lexer input)))))

(define (process-align x)
  (define xss (map (lambda (y) (string-split y "&")) (string-split x "\\\\")))
  (define lxss (map (lambda (v) (map labelled v)) xss))
  (define (has-label? lx) (if (text-with-lbl-label lx) #t #f))
  (define labels-detected? (if (ormap has-label? (flatten lxss)) #t #f))
  (define alignment-string "")
  (define (output-line lxs)
    (set! alignment-string 
          (if (cons? (cdr lxs))
              (if labels-detected? "r.l.n " "r.l ")
              (if labels-detected? "r.n " "l ")))
    (string-append 
     "@list[\n@f{"
     (string-trim #:repeat? #t (text-with-lbl-text (car lxs)))
     (if (cons? (cdr lxs))
         (string-append
          "}@f{"
          (string-trim #:repeat? #t (text-with-lbl-text (cadr lxs)))
          "}")
         "}")
     (if labels-detected?
         (if (has-label? (car lxs)) 
             (string-append "@label{" (text-with-lbl-label (car lxs)) "}")
             (if (and (cons? (cdr lxs)) (has-label? (cadr lxs)))
                 (string-append "@label{" (text-with-lbl-label (cadr lxs)) "}")
                 "\"\""))
         "")
     "\n]"))
  (let ([output-lines (map output-line lxss)])
    (string-append
     "@align[" 
     alignment-string 
     (apply string-append output-lines)
     "]")))

(define (text-to-str t)
  (apply
   string-append
   (map 
    (lambda (x) 
      (cond
       ([eq? x] 
        (let* ((l (labelled (eq-txt x)))
               (xlbl (text-with-lbl-label l))
               (xtxt (text-with-lbl-text  l)))
          (if xlbl
              (string-append "@equation[#:label \"" xlbl "\"]{" xtxt "}")
              (string-append "@equation{" xtxt "}"))))
       ([aln? x] (process-align (aln-txt x)))
       ([inline-formula? x] (string-append "@f{" (inline-formula-txt x) "}"))
       ([string? x] x)
       ))
    t)))

(let ((input (current-input-port)))
  (display 
   (text-to-str
    (latex-parser (lambda () (latex-lexer input))))))


