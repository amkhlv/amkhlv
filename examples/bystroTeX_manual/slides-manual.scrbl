#lang scribble/base
@(require racket scribble/core scribble/base scribble/html-properties)
@(require "defs.rkt" bystroTeX/common bystroTeX/slides (for-syntax bystroTeX/slides_for-syntax))
@; ---------------------------------------------------------------------------------------------------
@; User definitions:
@(bystro-set-css-dir (build-path 'same "css"))
@(define bystro-conf 
   (bystro (bystro-connect-to-server (build-path 'up "bystroConf.xml"))
           "formulas.sqlite"  ; name for the database
           "slides-manual" ; directory where to store the image files of formulas
           25  ; formula size
           (list 255 255 255) ; formula background color
           (list 0 0 0) ; formula foreground color
           2   ; automatic alignment adjustment
           0   ; manual alignment adjustment
           ))
@; This controls the single page mode:
@(define singlepage-mode #f)
@(bystro-def-formula "formula-enormula-humongula!")

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
@f{{\partial\phi\over\partial\sigma} = - {\partial W\over \partial \phi}} 
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

@slide["Installation Part I" #:tag "Installation" #:showtitle #t]{
BystroTeX consists of the frontend (Racket) and backend (Java). The Java part works
like a server. It is actually an HTTP server. It listens on some port on the @tt{localhost}.
We will start with @bold{setting up this server}.

@table-of-contents[]

@section{Install Java and Git}

First of all, you need to have installed @bold{Java 7 or 8} (because Java 6 will not work), including the JDK.
OpenJDK is OK. To verify that you have Java installed, type the commands:
@verb{
java -version
javac -version
}
They should say something like ``java version 1.7....'' and ``javac 1.7....''.

To install the server, you will need to install the following things on your computer: 

@itemlist[#:style 'ordered 
@item{@hyperlink["https://en.wikipedia.org/wiki/Git"]{git}}
@item{@hyperlink["https://en.wikipedia.org/wiki/Apache_Maven"]{mvn} and @hyperlink["https://en.wikipedia.org/wiki/SBT_(software)"]{sbt}}
]

@section{Build things}

Now execute the following commands:

@smaller{@tt{git clone https://github.com/amkhlv/latex2svg}}

@smaller{@tt{cd latex2svg}}

@smaller{@tt{git submodule init}}

@smaller{@tt{git submodule update}}

@smaller{@tt{cd jlatexmath}}

@smaller{@tt{mvn clean install}}

@smaller{@tt{cd ..}}

@smaller{@tt{sbt stage}}

This will take some time, as various libraries will have to be downloaded (and saved in @tt{~/.ivy2} and @tt{~/.sbt}).

@section{Run}
Our Java server will communicate to the Racket frontend some initial settings 
(including the anti-@hyperlink["https://www.owasp.org/index.php/Cross-Site_Request_Forgery_(CSRF)"]{CSRF} token) 
by writing them into an @tt{XML} file. You have to decide how it should name this file and where to put it.
Suppose that you decided to call it @tt{bystroConf.xml}, and choosen some directory where it will be:

@smaller{@tt{/path/to/bystroConf.xml}}

@comment{remember this location; you will need need it @seclink["SamplePresentation"]{in the following steps}}

Under this assumption, start the server by typing the following command:

@smaller{@tt{target/universal/stage/bin/latex2svg -DbystroFile=/path/to/bystroConf.xml -Dhttp.port=9749 -Dhttp.address=127.0.0.1}}

@comment{
If you want to use @tt{BibTeX}, add the option @tt{-Dbibfile=/path/to/your/file.bib}
}

@comment{
The port number @tt{9749} is also up to you to choose. The frontend will know it because it will be written (among other things) to @tt{/path/to/bystroConf.xml}
}

Now the server is running. 

@comment{
Notice that we specified the option @smaller{@tt{-Dhttp.address=127.0.0.1}}. Therefore the server
is only listening on a local interface (the ``loopback''); 
@hyperlink["http://stackoverflow.com/questions/30658161/server-listens-on-127-0-0-1-do-i-need-firewall"]{it is not possible to connect to it from the outside}.
However, it would be still 
@hyperlink["https://blog.jetbrains.com/blog/2016/05/11/security-update-for-intellij-based-ides-v2016-1-and-older-versions/"]{possible to attack it} 
from a running browser by 
@hyperlink["https://www.owasp.org/index.php/Cross-Site_Request_Forgery_(CSRF)"]{CSRF}.
Our defense is token and custom header. Should CSRF somehow succeed in spite of these measures, 
actually exploiting it would require a vulnerability in 
@hyperlink["https://github.com/opencollab/jlatexmath"]{JLaTeXMath}.
}
}

@slide["Installation Part II" #:tag "Installation2" #:showtitle #t]{
Now comes the frontend.

@table-of-contents[]

@section{Installing Racket}

You should start with installing @hyperlink["http://racket-lang.org/"]{@tt{Racket}} on your computer.
For example, on @tt{Debian} you should issue this command @bold{@clr["red"]{as root:}}
@verb{
aptitude install racket
}
This command will install the @tt{Racket} environment on your computer. Now we are ready to
install @tt{bystroTeX}.

@itemlist[
@item{@bold{For Windows users}: @smaller{You will have to manually install the @tt{dll} for the @tt{sqlite3}, please consult the Google}}
]

@section{Installing the BystroTeX library}

@bold{@clr["red"]{As a normal user}} (i.e. @bold{@clr["red"]{not}} root), exectute:

@verb|{
git clone https://github.com/amkhlv/amkhlv
cd amkhlv
raco pkg install --link bystroTeX/
}|

@comment{
Now you should be able to read the documentation manual in @tt{bystroTeX/doc/manual/index.html}, but it is not very useful. It is better to just follow examples.
}

@section[#:tag "sec:installing-the-executable"]{Installing the BystroTeX executable}

@verb|{
cd bystroTeX/
raco exe bystrotex.rkt
}|

This should create the executable file called @tt{bystrotex}. You should copy it to some location on your executable path (maybe @tt{/usr/local/bin/}).

@section{Building sample slides}

Now @spn[attn]{Now go to the sample folder}:

@verb{cd ../examples}

Remember your @tt{/path/to/bystroConf.xml} ? For sample slides to build, you need to symlink it to here:

@verb{ln -s /path/to/bystroConf.xml ./}

Now let us go to the sample slides directory:

@verb{cd bystroTeX_manual}

and proceed to the @seclink["SamplePresentation"]{next slide}...
}


@slide["Sample presentation" #:tag "SamplePresentation" #:showtitle #t]{

You should find that the sample folder contains (at least) the following files:

@tbl[#:orient 'hor @list[@list[@larger["Filename"] @larger["purpose"]]
@list[@tt{slides-manual.scrbl} "main source file"]
@list[@tt{defs.rkt} "additional definitions"]
@list[@tt{slide.css} "style of the regular slide"]
@list[@tt{slide-title.css} "style of the title slide"]
@list[@tt{misc.css} "various elements of style"]
@list[@tt{bystrotex.xml} @elem{@seclink["sec:xml-build-conf"]{build configuration} for this folder, basically a list of @tt{.scrbl} files and what to do with them}]
]]
To actually ``build'' the slideshow, just say:
@verb|{
bystrotex
}|
(remember @seclink["sec:installing-the-executable"]{you have put @tt{bystrotex}} on your executable path!)

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
But it should not be necessary, because the syntax should be clear from the source file of these pages. 
More examples can be found @hyperlink["https://github.com/amkhlv/BV"]{here}.

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
@f{{2\over 1 - x^2} = }  @v+[3 @f{1+x+x^2 + \ldots +}]
]@list[
"" @f{1-x+x^2- \ldots}
]
]
}|
@align[r.l
 @list[
@f{{2\over 1 - x^2} = }  @v+[3 @f{1+x+x^2 + \ldots +}]
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
@f{{2\over 1 - x^2} = }  @v+[3 @f{1+x+x^2 + \ldots +}] ""
]@list[
"" @f{1-x+x^2- \ldots} @label{SumOfGeometricProgressions}
]
]
}|
@align[r.l.n
 @list[
@f{{2\over 1 - x^2} = }  @v+[3 @f{1+x+x^2 + \ldots +}] ""
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
We use @hyperlink["http://forge.scilab.org/index.php/p/jlatexmath/"]{@tt{JLaTeXMath}} to produce @tt{svg} (or @tt{png}) files.
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

@verb|{
@slide[@elem{Putting @f{\epsilon} and @f{\delta} in the slide title} #:tag "FormulasInSlideTitle" #:showtitle #t]{

...

}
}|
}

@slide["Writing html in scribble" #:tag "InsertingHTML" #:showtitle #t]{

This @tt{HTML}:
@(tg img 
     #:attrs ([style "border:0;"]
              [src "http://www.123gifs.eu/free-gifs/flags/flagge-0544.gif"]
              [alt "flagge-0544.gif from 123gifs.eu"])
     "Flag of Brazil")

@verb[#:style @(make-style "comment" '())]|{
<img style="border:0;" src="http://www.123gifs.eu/free-gifs/flags/flagge-0544.gif" alt="flagge-0544.gif from 123gifs.eu">
Flag of Brazil
</img>
}|
should be inserted @hyperlink["http://docs.racket-lang.org/manual@bystroTeX/index.html#%28form._%28%28lib._bystro.Te.X%2Fcommon..rkt%29._tg%29%29"]{as follows}:
@verb[#:style @(make-style "comment" '())]|--{
@(tg img 
     #:attrs ([style "border:0;"]
              [src "http://www.123gifs.eu/free-gifs/flags/flagge-0544.gif"]
              [alt "flagge-0544.gif from 123gifs.eu"])
     "Flag of Brazil")

}--|

}

@slide["Emacs preview inspired by AUCTeX" #:tag "Emacs" #:showtitle #t]{
I use @hyperlink["https://github.com/greghendershott/racket-mode"]{racket-mode}.

The additional library
@hyperlink["https://github.com/amkhlv/amkhlv/blob/master/bystroTeX-preview.el"]{bystroTeX-preview.el} provides some rudimentary preview functionality similar to
@hyperlink["http://www.gnu.org/s/auctex/"]{AUCTeX}.

For example, see
@hyperlink["https://github.com/amkhlv/usr/blob/master/lib/emacs/emacs.el"]{my @tt{.emacs} file}
(search for @tt{racket-mode}).



}

@slide["@page[] vs @slide[]" #:tag "PageVsSlide" #:showtitle #t]{

In the multipage mode, there are two ways to start new page:

@hrule[]

@verb|{
       @slide["Title" #:tag ... #:showtitle #t]{
                   content
                   }
       }|

@hrule[]

and

@hrule[]

@verb|{
       @page["Title" #:tag ... #:showtitle #t]

       content
       }|

@hrule[]

The @tt|{@page[]}| is slightly different. The main difference is, there is no @tt|{ {} }|
around the content. This simplifies writing. But, on the other hand, @tt|{@page[]}| has
a disadvantage over @tt|{@slide[]}|:
@(itemlist
  @item{No way to use @tt|{@after-pause[]}| when using @tt|{@page[]}| instead of  @tt|{@slide[]}|}
  )
When using @tt|{@page[]}|, inside the content, use:
@verb|{
       @subpage[1 ...] instead of @section[...]
       @subpage[2 ...] instead of @subsection[...]
       @subpage[3 ...] instead of @subsubsection[...]
       }|
@tt|{@slide[]}| is more slideshow-oriented while @tt|{@page[]}| is more writeup-oriented.
          
}

@slide["Sections in slide" #:tag "Sections" #:showtitle #t]{
@table-of-contents[]
@section{Introduction}
@subsection{A slide can contain sections}
A slide can contain sections.
@div[redbox]{Restriction: the name of the section should not coincide with the tag of a slide. For example, 
suppose that you have a slide with  @tt|{#:tag "Happiness"}|; then, if you must have also @tt|{@section{Happiness}}|
then you must give it a tag, for example: @tt|{@section[#:tag "sectionHappiness"]{Happiness}}|
}

@subsection{Table of contents}

To list the table of contents, us @tt|{@table-of-contents[]}|

@spn[attn]{Attention:} on the @bold{title page}, use @tt|{bystro-toc[]}| and @bold{not}  @tt|{@table-of-contents[]}|

@section{Disadvantages}
Obviously, this is a bad idea when giving a talk:
@subsection{First disadvantage}
Audience will get confused.
@subsection{Second disadvantage}
Audience will start reading section titles instead of reading formulas.
@section{Advantages}
But it is good for wruteups.
}
@slide["Single page and printing" #:tag "SinglePage" #:showtitle #t]{
It is also possible to have everything on one single long @tt{html} page. 

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

This might be useful, for example, for @red{printing}. However, it is also easy
to print multiple pages, using the program called @hyperlink["http://wkhtmltopdf.org/"]{wkhtmltopdf}.
The only problem is to keep track of the ordering of pages. Notice that the @tt{index.html} has the
list of all your multiple pages, and this script will extract them and print the filenames sorted:

@verb[#:style @(make-style "comment" '())]|--{
#!/usr/bin/env racket
#lang racket 
(require xml xml/path racket/file)
(define x 
  (call-with-input-file "index.html"
    (lambda (inport) (xml->xexpr (document-element (read-xml inport))))))
(for ([ttl (se-path*/list '(a) x)] 
      #:when (and (string? ttl) (equal? (se-path* '(a #:class) x) "toptoclink")))
  (for ([fp (find-files 
             (lambda (f) (equal? (string-append ttl ".html") (path->string f))))])
    (write-string (path->string fp))
    (newline))
  (for ([fp (find-files 
             (lambda (f) (regexp-match 
                          (regexp (string-append ttl "_[0-9]+" ".html"))
                          (path->string f))))])
    (display (path->string fp))
    (newline)))
}--|



}

@slide["Automated build" #:tag "AutomatedBuild" #:showtitle #t]{
@table-of-contents[]

@section{Program to automatically build @tt{.scrbl} files}
If you have several @tt{.scrbl} files, executing the @tt{scribble} command could become tedious.
To address this, I wrote a simple build script @tt{bystrotex.rkt}. The best way to use it is
to compile first:
@verb|{raco exe bystrotex.rkt}|
This produces an executable file @tt{bystrotex}. Just put it somewhere on your @tt{PATH}
(for example in @tt{/usr/local/bin/}). 

@section[#:tag "sec:xml-build-conf"]{XML configuration file}
Notice that the sample folder @tt{examples/bystroTeX_manual} contains the file @tt{bystrotex.xml},
which describes the build configuration.
In that sample folder, execute the command:
@verb{bystrotex}
It will read the configuration from @tt{bystrotex.xml} and build accordingly.
To cleanup, say:
@verb{bystrotex -c}
The syntax of @tt{bystrotex.xml} is described in @tt{schemas/bystrotex.rnc}
Notice that @tt{<name>filename</name>} corresponds to the file @tt{filenames.scrbl}.

@section[#:tag "AvoidingConflicts"]{Avoiding conflicts}
A problem is likely to arise when you have more than one singlepage @tt{.scrbl} files in the same directory.
For example, suppose that you have @tt{file1.scrbl} and @tt{file2.scrbl} which are both singlepage.
The following example of @tt{bystrotex.xml}:
@verb|{
<scribbling>
    <name>file1</name>
</scribbling>
<scribbling>
    <name>file2</name>
</scribbling>
}|
is @spn[attn]{very wrong}, for the following reason. Both of them will create the formula
files, @italic{e.g.} @tt{1.svg} and @tt{2.svg} and this will result in confict!
(The formulas will get mixed up)
The correct configuration would be:
@verb|{
<scribbling>
    <name>file1</name>
    <dest>file1</dest>
</scribbling>
<scribbling>
    <name>file2</name>
    <dest>file2</dest>
</scribbling>
}|
or:
@verb|{
<scribbling>
    <name>file1</name>
    <formulas-dir>file1_formulas</formulas-dir>
</scribbling>
<scribbling>
    <name>file2</name>
    <formulas-dir>file2_formulas</formulas-dir>
</scribbling>
}|

@section{Building individual files}

You can also build individual files. To build two files, say:
@verb{bystrotex filename1 filename2}
This is equivalent to:
@verb{bystrotex filename1. filename2.scrbl}
The trailing dot is stripped to facilitate the use of TAB completion.

}

@slide["Automatic LaTeX → BystroTeX conversion" #:tag "LaTeX2BystroTeX" #:showtitle #t]{
I have tried to write 
@hyperlink["https://github.com/amkhlv/l2b2l"]{a program which converts a LaTeX document into BystroTeX}, 
for example replaces every @tt{$x$} with @tt|--{@f{x}}--|. This only covers a very limited subset of @tt{LaTeX}.
The problem is, every person writes @tt{LaTeX} in their own way.
For example, some people write @tt|{\beq}| instead of @tt|{\begin{equation}}|.
Then, the converter would have to be customized (by editing its source code).
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
@para{Formulas are mixed up}
@para{Do you have to singlepage @tt{scribble} files in the same directory?
See @seclink["AvoidingConflicts"]{Avoiding Conflicts}}
]@list[
@para{Execution of the command ``@tt{scribble ...}'' freezes}
@para{If your talk has many formulas, and this is the first time you are
executing the @tt{scribble} command, you may need to wait 1 min or so...

But sometimes it, really, freezes.
This may happen on slow machines. Do the following:
@linebreak[]
1. Stop @tt{scribble} (just @tt{Ctrl-C})
@linebreak[]
3. @tt{rm formulas.sqlite}
@linebreak[]
4. @tt{rm formulas/*}
@linebreak[]
6. Start your @tt{scribble} command again
}
]
]]
}

@slide["Using BibTeX" #:tag "BibTeX" #:showtitle #t]{
You should then @seclink["Installation"]{have started @tt{latex2svg}} with @tt{-Dbibfile=/path/to/your/file.bib}.
Also, you should add in the headers of your @tt{.scrbl} file:

@verb{(require bystroTeX/bibtex)}

Then you get the commands @tt|-{@cite{...}}-| and @tt|-{@bibliography[]}-|. They work as usual, except for:
@itemlist[#:style 'ordered
@item{you should explicitly put square bracket, @italic{e.g.}: @tt|-{[@cite{AuthorA:1989}]}-|}
@item{if you need several citations together, use: @tt|-{[@cite{AuthorA:1989},@cite{AuthorB:1990}]}-|}
]
This is slightly experimental.

}



@; ---------------------------------------------------------------------------------------------------
@disconnect[formula-database]
@(bystro-close-connection bystro-conf)
 
