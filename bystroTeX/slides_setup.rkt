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

We have a sample slide presentation, which you can use as a template for preparing your own talk. 

The question is, where should we put it?

The default location will be ~/bystro 
If you want to enter another location, please type the ABSOLUTE path
and press ENTER (on Linux you can use ~ for your home directory). 
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
)
