This directory contains the primary copy of the Piqi project documentation.

The up-to-date copy of documentation for the most recent stable version of Piqi
is published on the [project's website](http:/piqi.org/doc/).

Note that documents are written using
[Pandoc](http://johnmacfarlane.net/pandoc/) variation of Markdown syntax.


Building documentation
----------------------

**NOTE:** you need to have `pandoc` installed before running these commands.

`make man` command rebuilds `piqi(1)` man page.

`make html` command makes cross-linked offline HTML documentation with
`index.html` as a front page.

Running `make` is equivalent to `make man html`.

