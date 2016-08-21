
Automatic build
===============

This is a sample directory with several scribble files.

In order to build them, execute the command:

    bystrotex

In order to cleanup, say:

    bystrotex -c


Additional cleanup script
=========================

The configuration for the singlepage `sample-plain.scrbl` misses the destination.
Therefore it is built in this dir. In this case, it is @spn[attn]{important}
that the `sqlite` file is called `sample-plain_formulas.sqlite` (see the headers of `sample-plain.scrbl`).
