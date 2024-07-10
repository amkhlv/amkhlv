(module bibtex racket

  (require racket scribble/core scribble/base)
  (require bystroTeX/slides)

  (provide cite)
  (provide bibliography)
  
  (define items '()) ; list of citations
  
  (define (cite x)
    (when (empty? (for/list ([y items] #:when (equal? x (car y))) #t))
      (let ([xh (get-bib-from-zeromq x)])
        (displayln xh)
        (set! items (cons (cons x xh) items))))
    (elemref x x ))

  (define (not-null? x) (and x (not (eq? 'null x))))
  (define (prepend-comma x) (if (cons? x) (cons ", " x) x))
  (define (prepend-hspace x) (if (cons? x) (cons (hspace 1) x) x))
  (define (format-title x) (italic (string-trim x #px"\\{|\\}|\\s+" #:repeat? #t)))
  (define (format-journal bh)
    (if (not-null? (hash-ref bh 'journal #f))
        `(,@(let ([j (hash-ref bh 'journal #f)]) (if (not-null? j) `(,j) '()))
          ,@(let ([v (hash-ref bh 'volume #f)])  (prepend-hspace (if (not-null? v) `(,(bold v)) '())))
          ,@(let ([y (hash-ref bh 'year #f)])    (prepend-hspace (if (not-null? y) `(,(string-append "(" y ")")) '())))
          ,@(let ([p (hash-ref bh 'pages #f)])   (prepend-hspace (if (not-null? p) `(,(string-append "p." p)) '())))
          )
        '()))
  (define (format-eprint bh)
    `(,@(let ([p (hash-ref bh 'archiveprefix #f)]) (if (not-null? p) `(,(bold p ":")) '()))
      ,@(let ([x (hash-ref bh 'eprint #f)])        (if (not-null? x) `(,x) '()))))
  (define (format-doi bh)
    `(,@(let ([doi (hash-ref bh 'doi #f)]) (if (not-null? doi) `(,(bold "doi:") ,doi) '()))))
  (define (format-bibitem bh)
    (apply 
     elem
     `(,@(let ([a (hash-ref bh 'author #f)]) (if (not-null? a) `(,a ", ") '()))
       ,@(let ([t (hash-ref bh 'title #f)])  (if (not-null? t) `(,(format-title t)) '()))
       ,@(prepend-comma (format-journal bh))
       ,@(prepend-comma (format-eprint bh))
       ,@(prepend-comma (format-doi bh))
       )))
  (define (bibliography)
    (displayln items)
    (make-table 
     (make-style 
      #f
      `(,(make-table-columns `(,(make-style "bystro-bib-key-cell" '()) 
                               ,(make-style #f '()) 
                               ,(make-style "bystro-bib-value-cell" '())))))
     (for/list ([i (reverse items)])
       (list (para (elemtag (car i) (string-append "[" (car i) "]"))) 
             (para (hspace 1))
             (para
              (format-bibitem (cdr i))
              )))))
  )
