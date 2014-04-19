#lang scribble/base
@(require racket scribble/core scribble/base scribble/html-properties)
@(require (for-syntax "defs_for-syntax.rkt" (planet amkhlv/bystroTeX/slides_for-syntax)))
@(require "defs.rkt" (planet amkhlv/bystroTeX/common) (planet amkhlv/bystroTeX/slides))
@(require (only-in (planet jaymccarthy/sqlite) close))
@; ---------------------------------------------------------------------------------------------------
@; User definitions:
@(define bystro-conf 
   (bystro (find-executable-path "amkhlv-java-formula.sh")
           "formulas.sqlite"  ; name for the database
           "formulas" ; directory where to store .png files of formulas
           25  ; formula size
           (list 255 255 255) ; formula background color
           (list 0 0 0) ; formula foreground color
           2   ; automatic alignment adjustment
           0   ; manual alignment adjustment
           ))
@; This controls the single page mode:
@(define singlepage-mode #f)
@; ---------------------------------------------------------------------------------------------------
@(begin ;do not change anything here:
   (define (start-formula-database)
     (configure-bystroTeX-using bystro-conf)
     (bystro-initialize-formula-collection bystro-conf))
   (define formula-database (start-formula-database))
   (unless (bystro-formula-processor bystro-conf)
     (error "*** could not find executable for formula processing ***"))
   (define-syntax (syntax-setter x) (defines-syntax-for-formulas x))                
   (syntax-setter defineshiftedformula)
   (defineshiftedformula "formula-enormula-humongula!")
   (bystro-titlepage-init #:singlepage-mode singlepage-mode))
@; ---------------------------------------------------------------------------------------------------
@; AND HOPEFULLY SOME CONTENT:

@title{BystroTeX}

Mathematical slides
using the @hyperlink["http://docs.racket-lang.org/scribble/"]{@tt{scribble}} markup system

Andrei Mikhailov, IFT UNESP

@bystro-toc[]

@slide["Inserting mathematical formulas into html slides" #:tag "MathFormulasInSlides" #:showtitle #t]{

We can insert @f{W(\phi)} as an inline formula, or a display formula:
@equation[#:label "Lagrangian" ]{
{\cal L} = \left(\partial_{\mu}\phi,\partial^{\mu}\phi\right) - 
\left|\left| {\partial W(\phi)\over \partial\phi} \right|\right|^2
}
Or, formula in the box:
@longtbl[#:styless @'(((center) (right))) 
@list[@list[@nested[@redbox["padding:36px;8px;0px;8px;"]{
@f{\dot{\phi} = - {\partial W\over \partial \phi}} 
}] @nested{@label{SolitonEquation}}]]]
If we need, we can also insert @f-4{x^2} shifted down, 
or insert a shifted up and rescaled: @f+3+8{y^2}. @smaller{
@fsize+[-5]
We can also temporarily change the default size of the formulas. For example, this paragraph is
typeset using small font. The formula @f{yx^2} comes out smaller because we changed the default size.
After we restore the default size
@fsize=[]
@f{zx^2} is of the normal size. Some people like colored formulas: 
@bystro-bg[255 200 200] @bystro-fg[0 0 250]
@f{\; l^2 = a^2 + b^2}
@bystro-bg[255 255 255] @bystro-fg[0 0 0]
}

See the @seclink["Syntax"]{syntax part} for more examples.  
}

@after-pause{
@tabular[@list[@list[@para{
Besides the math-specific task of inserting formulas, many things can be done using
the @hyperlink["http://docs.racket-lang.org/scribble/"]{well documented} @tt{scribble} environment.
@linebreak[]
For example, one can include pictures, as illustrated. Flying kite could be an allegory for slide presentations.}
@smaller-2{@(image (string->path "snapshots/kite.png") #:scale 0.5)
@hyperlink["http://openclipart.org/detail/67597/child-with-a-kite-by-laobc"]{openclipart.org}}]]]
}

@remove-slide[]

@after-pause{
@clr["red"]{ Also supported pause and removal of slides.}
}

@slide["Jumping to references" #:tag "JumpingReferences" #:showtitle #t]{
Remember the formula (@ref{Lagrangian})? Clicking on
(@ref{Lagrangian}) brings you to that formula.

It is also possible to jump to a particular slide, for example 
@seclink["Installation"]{jump to ``Installation''}.

Sometimes you might want to press the ``up'' link, which will bring you to the title page.
The title page has the list of contents, so you can jump to particular slides from there.
}

@slide["Installation" #:tag "Installation" #:showtitle #t]{
You should start with installing @hyperlink["http://racket-lang.org/"]{@tt{Racket}} on your computer.
For example, on @tt{Debian} you should issue this command @bold{@clr["red"]{as root:}}
@verb{
aptitude install racket
}
This command will install @tt{Racket} environment on your computer.

You also need to have installed @bold{Java 7} (because Java 6 will not work), including the JDK.
OpenJDK is OK. To verify that you have Java installed, type the commands:
@verb{
java -version
javac -version
}
They should say something like ``java version 1.7....'' and ``javac 1.7....''.

@div[comment]{
In particular, on Debian Wheezy, the default version of Java is actually 1.6, but 1.7 is also available.
You should switch to 1.7, using the command @tt{update-java-alternatives}.
}

Next you should install @tt{bystroTeX} using this command @bold{@clr["red"]{as normal user}} 
(i.e. @bold{@clr["red"]{not}} root):
@verb{
racket -e '(require (planet amkhlv/bystroTeX/slides_setup))'
}
This command will take some time, because it has to download and compile things.
It will talk back to you, asking some questions and giving further instructions. 
In particular, it will ask you to choose a folder where to store the Java staff, and also the
folder where a sample slide presentation is stored.
After the installation, @clr["red"]{go to that sample folder}.
}


@slide["Sample presentation" #:tag "SamplePresentation" #:showtitle #t]{

You should find that the sample folder contains (at least) the following files:

@tbl[#:orient 'hor @list[@list[@larger["Filename"] @larger["purpose"]]
@list[@tt{slides-manual.scrbl} "main source file"]
@list[@tt{defs.rkt} "additional definitions"]
@list[@tt{defs_for-syntax.rkt} "syntax preferences"]
@list[@tt{slide.css} "style of the regular slide"]
@list[@tt{slide-title.css} "style of the title slide"]
@list[@tt{misc.css} "various elements of style"]
]]

The command to actually ``build'' the slideshow is:
@verb|{
scribble --htmls slides-manual.scrbl 
}|
This command may take some time when you run it for the first time. There are two possible
outcomes:
@itemlist[#:style 'ordered
@item{If something goes wrong, see the @seclink["Troubleshooting"]{Troubleshooting section}.}
@item{If the command succeeds, it will generate the @tt{html} files in the new folder called @tt{slides-manual}.}
]
Then you can view it in @tt{Firefox}:
@verb|{
firefox slides-manual/index.html
}|
}

@slide["Adjusting fontsizes, width and other style" #:tag "AdjustCSS" #:showtitle #t]{
Most of things are done in 2 @tt{css} files, @tt{slide.css} and @tt{slide-title.css}.
The first file is for the regular slide, and the second for the titlepage.

@smaller-2{@bold{@clr["red"]{Note}}:
I am talking about the files in the root of the sample folder. When you 
@seclink["SamplePresentation"]{compile}, they get copied into the subfolder 
@tt{slides-manual/index.html}. Everything in that subfolder (in particular @tt{css} files) 
is auto-generated. Therefore, you should not edit anything there!
}

How to figure out which element of the slide corresponds to which section of the @tt{css} file?
Firefox has the ``style inspector'' tool which reveals names. 
It is activated by pressing @tt{Ctrl-Shift-I}. 
If you press on the ``style'' tab, it will even show you from which line of the @tt{css} file
came this or that element on the page! @smaller-2{(To turn it off press @tt{Ctrl-Shift-I} again.)}

Notice that the width is controlled by the @tt{width} parameter of @tt{.maincolumn}.

}

@slide["Will it fit on the projector screen?" #:tag "FitProjector" #:showtitle #t]{
This is a very important question. Before giving a talk, try to figure out the projector resolution.
(I think most of them are 1024x768.) I once got the projector replaced just before my talk, 
so I found myself trying to @seclink["AdjustCSS"]{edit the .css files} right in front of the audience. 
@smaller-2{(Nice exercise!)}

Linux has a command called @tt{xte}, which can position the mouse cursor at a given point 
on the screen. For example:
@verb|{
xte 'mousemove 1024 768'
}|
Maybe this can be used to check if your slide will fit.

The easiest way to adjust the width of the slide is by using the @tt{width} 
parameter of @tt{.maincolumn}.

@smaller{
Alternatively, you can change the font sizes. If you do decide to change the font sizes,  you 
might also need to adjust the formula size in the headers of the @tt{.scrbl} file, and
perhaps in the @tt|--{@fsize[]}--| function.
}
}

@slide["Basic syntax" #:tag "Syntax" #:showtitle #t]{
You might want to read @hyperlink["http://docs.racket-lang.org/scribble/reader.html"]{basic Scribble documentation},
but it should not be necessary. Just look at the source file and proceed by analogy. 
Also have a look at the ``docs'' link @hyperlink["http://planet.racket-lang.org/display.ss?package=bystroTeX.plt&owner=amkhlv"]{on PLaneT}.
More examples can be found @hyperlink["https://github.com/amkhlv/dgcm"]{here}.

@table-of-contents[]
@section{Simple formulas}
To insert formula @f{x^2/y^2}, type:
@verb|{@f{x^2/y^2}}|
Curly brackets inside are usually not a problem: for @f{y_{ij}} just type 
@verb|{@f{y_{ij}}}|
it works. If however something goes wrong, you might want to use better protection:
@verb|---{@f|{y_{ij}}| or even @f|-{y_{ij}}-| }---|

@div[comment]{Whether you need to use @tt|{@f|-{...}-|}|, or @tt|{@f{...}}| is enough, depends on 
the structure of parentheses inside your formula. @bold{If parentheses are well-balanced} then
@tt|{@f{...}}| is enough. If not, then better protection is needed. For example, if the formula
is: @f-4|{v = \left\{\begin{array}{l} u \mbox{ if } u \geq 0 \cr -u \mbox{ if } u < 0\end{array}\right.}|,
then you absolutely need to use @tt|{@f|-{...}-|}|, since the @f|{\{}| is unbalanced}

There is also the display-style @tt|--{@equation{...}}--| which allows formula labeling using
@tt|--{@equation[#:tag "FormulaName"]{...}}--|.


It is also possible to manually align the formulas, for example 
@tt|--{@f+4{x^2}}--| produces @f+4{x^2} and @tt|--{@f-7{x^2}}--| gives @f-7{x^2}.
There is also zoomed @tt|--{@f+0+7{x^2}}--| which gives @f+0+7{x^2} and zoom
with align @tt|--{@f-5+7{x^2}}--| which gives @f-5+7{x^2}.

The command @tt|--{@fsize[20]}--| changes the formula size to 20pt, the command @tt|--{@fsize[]}--|
or equivalently @tt|--{@fsize=[]}--|
returns back to the previous size (but you can not nest them, there is not stack of sizes).
Actually I recommend to use instead the command @tt|--{@fsize+[5]}--| which changes the
size relatively to the base size. This  will scale better if you will have to suddenly
@seclink["FitProjector"]{change the resolution} 3 minutes before your talk. 
To decrease the size, use @tt|--{@fsize+[@-[5]]}--| or equivalently  @tt|--{@(fsize+ (- 5))}--|.
Both @tt|{@fsize[]}| and @tt|{@fsize+[]}| have an optional second argument, which modifies
the vertical base alignment.


@section{Multiline formulas}
Example:
@verb|{
@align[r.l
 @list[
@f{{2\over 1 - x^2} = }  @f{1+x+x^2 + \ldots +}
]@list[
"" @f{1-x+x^2- \ldots}
]
]
}|
produces:
@align[r.l
 @list[
@f{{2\over 1 - x^2} = }  @f{1+x+x^2 + \ldots +}
]@list[
"" @f{1-x+x^2- \ldots}
]
]
The only problem is, there is a small alignment defect. To fix it, do this:
@verb|{
@align[r.l
 @list[
@f{{2\over 1 - x^2} = }  @v+[8 @f{1+x+x^2 + \ldots +}]
]@list[
"" @f{1-x+x^2- \ldots}
]
]
}|
@align[r.l
 @list[
@f{{2\over 1 - x^2} = }  @v+[8 @f{1+x+x^2 + \ldots +}]
]@list[
"" @f{1-x+x^2- \ldots}
]
]
Notice that in the first line stands the symbol "r.l" which defines the alignment (right, then left). The "v+" is a padding, 
it serves for vertical adjustment, see the 
@hyperlink["http://planet.racket-lang.org/package-source/amkhlv/bystroTeX.plt/6/3/planet-docs/manual/index.html"]{manual page}.

The numbered version of the same formula will be
@verb|{
@align[r.l.n
 @list[
@f{{2\over 1 - x^2} = }  @v+[8 @f{1+x+x^2 + \ldots +}] ""
]@list[
"" @f{1-x+x^2- \ldots} @label{SumOfGeometricProgressions}
]
]
}|
@align[r.l.n
 @list[
@f{{2\over 1 - x^2} = }  @v+[8 @f{1+x+x^2 + \ldots +}] ""
]@list[
"" @f{1-x+x^2- \ldots} @label{SumOfGeometricProgressions}
]
]
Notice that the alignment symbol is now r.l.n, we added letter n for the number. The function @tt|{@label[]}| is defined in the slides’ header.

@section{Fun with Unicode}
To get @f{A\otimes B} just type: @tt|{@f{A⊗B}}|. In other words, we can use the Unicode symbol ⊗ instead of @tt{\otimes} in formulas.

@section{Color in formulas}

@verb|{
@bystro-bg[255 200 200] @bystro-fg[0 0 250]
@f{\; l^2 = a^2 + b^2}
@bystro-bg[255 255 255] @bystro-fg[0 0 0]
}|

gives @bystro-bg[255 200 200] @bystro-fg[0 0 250]
@f{\; l^2 = a^2 + b^2}
@bystro-bg[255 255 255] @bystro-fg[0 0 0] 

@section{Some technical details}
We use @hyperlink["http://forge.scilab.org/index.php/p/jlatexmath/"]{@tt{JLaTeXMath}} to produce @tt{png} files.
We had some problems with large size integral signs: @f{\int {f\over g}  dt} and large size 
brackets: @f{\left( {x\over y} \right)}. As you can see, they come out a bit weird. 
It seems that this is caused by a general bug in @tt{OpenJDK} (which is the default implementation
of Java on Debian). The situation is discussed 
@hyperlink["http://forge.scilab.org/index.php/p/jlatexmath/issues/676/"]{here}.
If this becomes a problem, use 
@hyperlink["http://www.oracle.com/technetwork/java/javase/downloads/index.html"]{Sun JDK} 
instead of @tt{OpenJDK}. (The page you are reading now was prepared using @tt{OpenJDK}.)

@div[redbox]{
This slide is very tall and will probably go below the bottom of the display area, so you
need to  scroll down to read it.
This is intentional. Sometimes you have to show a long list of formulas.
Scrolling is easier on the audience than jumping from page to page.
}
}

@slide[@elem{Putting @f{\epsilon} and @f{\delta} in the slide title} #:tag "FormulasInSlideTitle" #:showtitle #t]{
@verb|----{
@slide[@elem{Putting @f{\epsilon} and @f{\delta} in the slide title} #:tag "FormulasInSlideTitle" #:showtitle #t]{
@verb|---{
@slide[@elem{Putting @f{\epsilon} and @f{\delta} in the slide title} #:tag "FormulasInSlideTitle" #:showtitle #t]{
@verb|--{
@slide[@elem{Putting @f{\epsilon} and @f{\delta} in the slide title} #:tag "FormulasInSlideTitle" #:showtitle #t]{
@verb|-{
@slide[@elem{Putting @f{\epsilon} and @f{\delta} in the slide title} #:tag "FormulasInSlideTitle" #:showtitle #t]{
@verb|{
@slide[@elem{Putting @f{\epsilon} and @f{\delta} in the slide title} #:tag "FormulasInSlideTitle" #:showtitle #t]{

...... дурная бесконечность или вещь в себе ? ......

}
}|
}
}-|
}
}--|
}
}---|
}
}----|
}
@slide["Writing html in scribble" #:tag "InsertingHTML" #:showtitle #t]{

What if we want to insert, for example, a remote animated gif
from  @hyperlink["http://www.123gifs.eu/free-gifs"]{123gifs.eu} ?

As far as I understand, there is no way to directly insert a raw @tt{html} code in scribble. 
Instead, we have to rewrite our @tt{html} 
the @hyperlink["http://docs.racket-lang.org/scribble/index.html"]{scribble way}. For example, this @tt{html}:

@(element 
     (make-style #f (list
                     (alt-tag "img")
                     (attributes 
                      (list
                       (cons 'style "border:0;")
                       (cons 'src "http://www.123gifs.eu/free-gifs/flags/flagge-0544.gif")
                       (cons 'alt "flagge-0544.gif from 123gifs.eu")
                       ))))
   "Flag of Brazil"
   )


@verb|{
<img style="border:0;" src="http://www.123gifs.eu/free-gifs/flags/flagge-0544.gif" alt="flagge-0544.gif from 123gifs.eu">
Flag of Brazil
</img>
}|

will become:

@verb|--{
@(element 
     (make-style #f (list
                     (alt-tag "img")
                     (attributes 
                      (list
                       (cons 'style "border:0;")
                       (cons 'src "http://www.123gifs.eu/free-gifs/flags/flagge-0544.gif")
                       (cons 'alt "flagge-0544.gif from 123gifs.eu")
                       ))))
   "Flag of Brazil"
   )
}--|

}

@slide["Notes for emacs users" #:tag "Emacs" #:showtitle #t]{
If you use @tt{emacs}, I would highly recommend installing the @tt{emacs} scripts
from @hyperlink["http://www.neilvandyke.org"]{www.neilvandyke.org}. Not as powerful as
@hyperlink["http://www.gnu.org/s/auctex/"]{AUCTeX}, but still very cool!

Some more staff in @tt{.emacs}, if you wish:

@verb|--{
(add-outline 'scribble-mode-hook)
(add-hook 'scribble-mode-hook '(lambda () (setq outline-regexp "@section\\|@subsection\\|@subsubsection\\|@slide")))
(defface scribble-slide-face
  '((((class color) (background dark)) (:inherit variable-pitch :family "Terminus" :foreground "khaki2" :weight bold :height 1.2)))
  "Basic face for highlighting the scribble slide title.")
(add-hook 'scribble-mode-hook '(lambda () (font-lock-add-keywords 'scribble-mode
      '(("@slide\\[\"\\(.*?\\)\".*\\]" 1 'scribble-slide-face prepend)
        ("@slide\\[@elem{\\(.*?\\)}.*\\]" 1 'scribble-slide-face prepend)
        ("@\\(after-pause\\)" 1 'font-lock-warning-face prepend)
        ("@\\(slide\\)" 1 'font-lock-warning-face prepend)))))
}--|

--- for more syntax highlighting
}

@slide["Sections in slide" #:tag "Sections" #:showtitle #t]{
@table-of-contents[]
@section{Introduction}
A slide can contain sections
@section{Disadvantages}
Obviously, this is a bad idea when giving a talk:
@subsection{First disadvantage}
Audience will get confused.
@subsection{Second disadvantage}
Audience will start reading section titles instead of reading formulas.
@section{Advantages}
But it could be good for something else.
}
@slide["Single page and printing" #:tag "SinglePage" #:showtitle #t]{
It is also possible to have everything on one single long @tt{html} page. This is useful, for example,
for @red{printing}. 

For that, two things have to be done:
@itemlist[#:style 'ordered
@item{Change @tt|{@(define singlepage-mode #f)}| to @tt|{@(define singlepage-mode #t)}| in the 
headers of your @tt{.scrbl} file}
@item{Different method of compilation. 
Instead of compiling as we explained @seclink["SamplePresentation"]{here}, do this:
@itemlist[#:style 'ordered
@item{Create new directory: @verb{mkdir singlepage}}
@item{Now compile: @verb|{scribble --dest singlepage slides-manual.scrbl}|
}]}]
}

@slide["Automatic LaTeX → BystroTeX conversion" #:tag "LaTeX2BystroTeX" #:showtitle #t]{
I have tried to write a program which converts a LaTeX document into BystroTeX, for example
replaces every @tt{$x$} with @tt|--{@f{x}}--|. This is very difficult to do correctly,
because LaTeX is extremely difficult to parse. What I did is a very naive zeroth approximation.

The resulting script is @tt{l2b.rkt} which is in the sample directory. It accepts LaTeX on @tt{stdin}
and outputs BystroTeX to @tt{stdout}. 
To compile it, execute the following command:
@verb{raco exe l2b.rkt}
This will create the executable called @tt{l2b}. Example of use:
@verb{echo 'example: $1$' | ./l2b}
The output should be:
@verb|-{example: @f{1}}-|

The problem is, every person writes LaTeX in their own way.
For example, some people write @tt|{\beq}| instead of @tt|{\begin{equation}}|.
So, if you want to use a converter like this, you would probably have to modify it to fit your 
own LaTeX style.

}

@slide["Updating bystroTeX" #:tag "Updating" #:showtitle #t]{
If you already have @tt{bystroTeX} installed, but want to update to the latest
version. Go to @hyperlink["http://planet.racket-lang.org/display.ss?package=bystroTeX.plt&owner=amkhlv"]{the Planet page}.
Somewhere at the top of the page, you will find the following line:
@verb|{
To load: 	(require (planet amkhlv/bystroTeX:5:0/slides_setup))
}|
except for instead of @tt{bystroTeX:5:0} there will be @tt{bystroTeX:m:n} where @tt{m:n} is the latest version number.
Then execute this command:
@verb|{
racket -e '(require (planet amkhlv/bystroTeX:m:n/slides_setup))'
}|
This will repeat the installation procedure and update to the latest version. 

I recommend cleaning up the directories @tt{~/bystro} and @tt{~/java} (or whatever you called them
@seclink["Installation"]{during the previous installation}) before updating. This is because I may
have some changes both in the sample slides and in the Java part.
}

@slide["Troubleshooting" #:tag "Troubleshooting" #:showtitle #t]{
You need @bold{Java 7} (because Java 6 will not work). OpenJDK should be OK. 
Java errors are dumped to @tt{server-error.txt}. 

@tbl[#:orient 'hor @list[
 @list[
@para{Something is very wrong} 
@para{Check if the @tt{singlepage-mode} parameter is set up 
correctly in the headers, as explained @seclink["SinglePage"]{here}.}
]@list[
@para{Some formulas look ugly}
@para{Use Sun Java as explained @seclink["Syntax"]{here}}
]@list[
@para{First time execution of the ``@tt{scribble ...}'' command takes about half a minute, and then error message like @tt{read-xml: parse-error: expected root element ...}}
@para{Looks like a Java error, cannot find @tt{jlatexmath.jar} or something; see the contents of the log file @tt{server-error.txt}}
]@list[
@para{Execution of the command ``@tt{scribble ...}'' freezes}
@para{If your talk has many formulas, and this is the first time you are
executing the @tt{scribble} command, you may need to wait 1 min or so...

But sometimes it, really, freezes.
This may happen on slow machines. Do the following:
@linebreak[]
1. Stop @tt{scribble} (just @tt{Ctrl-C})
@linebreak[]
2. @tt{rm bystrotex.fifo}
@linebreak[]
3. @tt{rm formulas.sqlite}
@linebreak[]
4. @tt{rm formulas/*}
@linebreak[]
5. @tt{pkill -n java}
@linebreak[]
6. Start your @tt{scribble} command again
}
]
]]
}

@slide["Under the hood" #:tag "UnderTheHood" #:showtitle #t]{
@image{svg/under-the-hood.svg}

The @bold{backend} can be replaced with any program which has the following properties:
@itemlist[#:style 'ordered
@item{It should wait on @tt{stdin} for a text string of the form:
@verb|{
<formula size="25" bg="255:255:255" fg="0:0:0" filename="formulas/3.png"> \sum_{n&gt;0} {x^n\over n} </formula>
}|
(this is @hyperlink["http://en.wikipedia.org/wiki/XML"]{XML})
}
@item{Upon the receipt of such a text string, it should do two things:
@itemlist[
@item{Write the picture of the formula in the @tt{png} format to @tt{formulas/3.png}}
@item{Print on @tt{stdout} the text like this:
@verb|{
<report><depth>7</depth></report>
}|
which reports the depth (@italic{i.e.} vertical offset) of the formula}
]}
]
The name of such a program (in our case it is @tt{amkhlv-java-formula.sh}) should be
specified (among other things) at the top of the slides header in these lines:
@verb|{
@(define bystro-conf 
   (bystro (find-executable-path "amkhlv-java-formula.sh")
           "formulas.sqlite"  ; name for the database
           "formulas" ; directory where to store .png files of formulas
           25  ; formula size
           2   ; automatic alignment adjustment
           0   ; manual alignment adjustment
           ))
}|
Our backend which we are using contains a @hyperlink["http://www.linuxjournal.com/article/2156"]{Linux pipe},
which prevents it from being used on Windows. 
}
@slide["Why not Beamer?" #:tag "Beamer" #:showtitle #t]{
There is an excellent tool called @hyperlink["http://en.wikipedia.org/wiki/Beamer_(LaTeX)"]{Beamer},
which is based on @tt{LaTeX} and can be used with @hyperlink["http://www.gnu.org/s/auctex/"]{AUCTeX}. 
The problem is, I don't like @tt{TeX} and @tt{LaTeX}. This is, of course, a matter of personal preference.

Also, Scribble produces @tt{.html} files. I find @tt{.html} format more convenient for giving presentations
than the @tt{Beamer}'s format @tt{.pdf}. 

@elem[#:style @bystro-elemstyle["text-decoration:blink;"]]{Most importantly: Beamer won't give you blinking text.}

@div[comment]{It turns out, that Firefox @hyperlink["https://bugzilla.mozilla.org/show_bug.cgi?id=857820"]{disabled blink-effect},
what a pity!}
}

@slide["From TeX to HTML" #:tag "FromTeXtoHTML" #:showtitle #t]{
Seriously,
@redbox["padding:8px;8px;8px;8px;"]{
@larger{I think that we should switch from TeX to HTML}
}
@itemlist[
@item{TeX is optimized for the quality of typesetting. This is not very important for us.}
@item{HTML is all about @spn[attn]{creating links}. This is exactly what we need. 
After all, scientific publishing is for @spn[attn]{linking} ideas.}
@item{Practically, TeX is very hard to learn, and hard to use properly.
With the right markup tools (perhaps this project?) HTML should be easier.}
@item{Learning the syntax of LISP (@hyperlink["http://racket-lang.org/"]{Racket}) is pleasure. 
The syntax of TeX/LaTeX is pain.}
]

}

@close[formula-database]
