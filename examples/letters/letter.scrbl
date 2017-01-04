#lang scribble/base
@(require racket scribble/core scribble/base scribble/html-properties)
@(require "defs.rkt" bystroTeX/common bystroTeX/slides (for-syntax bystroTeX/slides_for-syntax))
@; ---------------------------------------------------------------------------------------------------
@; User definitions:
@(bystro-set-css-dir (build-path 'same "css"))

@; HERE:
@(define dst (bystro-get-cl-argument "dest"))
@(define to-whom (bystro-get-cl-argument "to_whom"))
@(define address (bystro-get-cl-argument "address"))
@; ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

@(define bystro-conf 
   (bystro (bystro-connect-to-server (build-path 'up "bystroConf.xml")) 
           "formulas.sqlite"  ; name for the database
           dst ; directory where to store the image files of formulas
           25  ; formula size
           (list 255 255 255) ; formula background color
           (list 0 0 0) ; formula foreground color
           2   ; automatic alignment adjustment
           0   ; manual alignment adjustment
           ))
@(define singlepage-mode #t)
@(bystro-def-formula "formula-enormula-humongula!")

@; AND HOPEFULLY SOME CONTENT:


@tg[table #:attrs ([border "0"] [width "100%"])
@tg[tr 
    @tg[td #:attrs ([style "text-align:left;"]) 
        @tg[table 
            (for/list ([x (string-split address "\n")]) 
              (tg tr (tg td x)))]
       ]
    @tg[td #:attrs ([style "text-align:right;"])
        @hyperlink["http://www.example.com/"]{Independent Researcher}
        @linebreak[]
        "Remote University"
        @linebreak[]
        @hyperlink["mailto:example@example.com"]|{example@example.com}|
        ]]]

@larger{Dear @(elem to-whom)! }

please read the enclosed text presenting my idea about the interpretation of the Higgs boson as a wormhole in graphene-lattice.


@; ---------------------------------------------------------------------------------------------------
@(bystro-close-connection bystro-conf)
@disconnect[formula-database]

  
