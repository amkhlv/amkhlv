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

(module common racket

(require mzlib/etc)
(require racket/system)

(let ((text #<<EOT

BystroTeX setup
===============
You need to have installed on your computer the program called ``javac''
If you do have it, press ENTER now to continue
EOT
)) (display text))
(display (read-line))
(let ((text #<<EOT

We have a sample slide presentation, which you can use as a template 
for preparing your own talk. 
The question is, where should we put it?
The default location will be ~/bystro 
If you want to enter another location, please type the ABSOLUTE path
and press ENTER (you can use ~ for your home directory). 
Or, if the default location is OK, then just press ENTER:

EOT
)) (display text))
(let* ((userinput (read-line))
       (sampledir 
        (if ((string-length userinput) . > . 0)
            (path->string (expand-user-path (string->path userinput)))
            (path->string (expand-user-path (string->path "~/bystro"))))))
  (unless (directory-exists? (string->path sampledir))
    (make-directory (string->path sampledir)))
  (system (string-append 
           "cp -a " 
           (path->string (this-expression-source-directory)) 
           "/example-slides/* " 
           sampledir 
           "/")))
(let ((text #<<EOT

Now we have to install some Java files.
Please type the ABSOLUTE path of the directory where you want to keep those Java files,
and press ENTER. Again, you can use ~ to denote your home dir. If you just press ENTER,
then the default location ~/java will be used:

EOT
)) (display text))
(define userinput (read-line))
(define javadir 
  (if ((string-length userinput) . > . 0)
      (path->string (expand-user-path (string->path userinput)))
      (path->string (expand-user-path (string->path "~/java")))))
(unless (directory-exists? (string->path javadir))
  (make-directory (string->path javadir)))
(display "\n\n")

(define javapath (string->path javadir))

(printf #<<EOT

Go to   http://forge.scilab.org/index.php/p/jlatexmath/downloads/ 
and download the latest jlatexmath-x.x.x.jar
Save it to the folder ~a 
WHEN DONE press ENTER
EOT
javadir)
(display (read-line))

(define jarpath-jlatexmath 
  (let ([jlatexps (find-files 
                   (lambda (x) 
                     (regexp-match #rx".*/jlatexmath-.*\\.jar$" (path->string x)))
                   (string->path javadir))])
    (unless (pair? jlatexps) (error (format #<<EOT

*** ERROR ***
Could not find the jlatexmath jar file in ~a
Something went wrong with the download?
EOT
javadir)))
    (when   (pair? (cdr jlatexps)) (error (format #<<EOT

*** ERROR ***
More than one jar file found in ~a
Please cleanup!
EOT
javadir)))
    (car jlatexps)))

(display (path->string (this-expression-source-directory)))

(system (format 
         "cp '~aLaTeXServer.java' '~a'"
         (path->string (this-expression-source-directory))
         javadir))
(system (format
         "cp '~aserver-control.sh' '~a'"
         (path->string (this-expression-source-directory))
         javadir))
(system (format
         "javac -classpath '~a' '~a/LaTeXServer.java'" 
         (path->string jarpath-jlatexmath)
         javadir))
(let ((out (open-output-file (build-path javapath "amkhlv-java-formula.sh") #:exists 'replace)))
  (fprintf 
   out
   #<<EOT
#!/usr/bin/env bash
export AMKHLV_JAVA_PATH='~a'
export   JLATEXMATH_JAR='~a'
'~a/server-control.sh'

EOT
(path->string javapath)
(path->string jarpath-jlatexmath)
(path->string javapath))
(close-output-port out))

(system (format "chmod +x ~a" (path->string (build-path javapath "amkhlv-java-formula.sh"))))
(system (format "chmod +x ~a" (path->string (build-path javapath "server-control.sh"))))
(printf #<<EOT

Finally, one more thing to do.
The directory ~a contains the file: 
 
 amkhlv-java-formula.sh 

You should move it to some place on your $PATH, and make sure that it is executable.
After this is done, BystroTeX is ready to use.

EOT
(path->string javapath))

)
