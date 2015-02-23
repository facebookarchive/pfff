
                              pfff

[![Build Status](https://travis-ci.org/facebook/pfff.svg?branch=master)](https://travis-ci.org/facebook/pfff)

pfff is a set of tools and APIs to perform static analysis, code
visualizations, code navigations, or style-preserving source-to-source
transformations such as refactorings on source code. There is good
support for C, Java, Javascript and PHP. There is also preliminary
support for other languages such as C++, Rust, C#, Html, CSS, Erlang,
Lisp, Haskell, Python, OPA and SQL. There is also very good support for
OCaml code so that the framework can be used on the code of pfff
itself.

For each languages there are mainly 2 libraries, for instance
parsing_php.cma and analysis_php.cma, that you can
embed in your own application if you need to process PHP code. See the
demos/ directory for example of use of the pfff API. See also
docs/manual/Parsing_xxx.pdf and docs/manual/Analyzis_xxx.pdf for
more documentation on how to use or extend pfff.

pfff is also made of few tools:
 - pfff, which allows to test the different parsers on a single file
 - scheck, a bug finder
 - stags, an Emacs tag generator
 - sgrep, a syntactical grep
 - spatch, a syntactical patch
 - codemap, which is a gtk and cairo based source code
   visualizer/navigator/searcher leveraging
   the information computed previously by pfff_db and codegraph.
 - codegraph, a source code indexer and package/module/class
   dependency visualizer
 - codequery, an interactive tool a la SQL to query information
   about the structure of a codebase using Prolog as the query engine
 - pfff_db, which does some global analysis on a set of source files and
   store the data in a marshalled form in a file somewhere (e.g. /tmp/db.json)

For more information, look at the pfff wiki here:
 http://github.com/facebook/pfff/wiki/Main
as well as the docs/manual/ directory.

Usage for pfff:
-----------------

   $ ./pfff -parse_php demos/foo.php

or

   $ ./pfff -dump_php demos/foo.php

You can also look at ./pfff --help

Usage for pfff_db:
-------------------

   $ ./pfff_db -lang ml -o /tmp/pfff.json ~/pfff

to analyze all the .ml and .mli files under ~/pfff and store metadata
information (the database) in /tmp/pfff.json

Usage for codemap:
------------------------

  $ ./codemap ~/pfff

This should launch a gtk-based GUI that allows you to visualize
source code and perform some code search.

Usage for codegraph:
------------------------

  $ ./codegraph -lang cmt -build ~/pfff

to generate a graph_code.marshall file in ~/pfff containing
all dependency information about the pfff codebase using the
typed bytecode .cmt files generated during the compilation of pfff.

  $ ./codegraph ~/pfff

This should launch a gtk-based GUI that allows you to visualize
source code dependencies.

More information
----------------------

Look at the pfff wiki here: http://github.com/facebook/pfff/wiki/Main
