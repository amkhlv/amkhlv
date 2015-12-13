
Example of templates
====================

This example illustrates the injection of variables via the configuration file.
Notice that there is only one scribble file: `letter.scrbl` (the "template"). 
But `bystrotex.xml` contains two items. This way we can obtain several HTML outputs,
slightly different from each other, from one scribble source. 

Automatic build
===============

This is a sample directory with several scribble files.

In order to build them, execute the command:

    bystrotex

In order to cleanup, say:

    bystrotex -c
