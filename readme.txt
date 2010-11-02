
                              pfff

pfff is mainly an OCaml API to write static analysis, dynamic
analysis, code visualizations, code navigations, or style-preserving
source-to-source transformations such as refactorings on source
code. For now the effort is focused on PHP but there is preliminary
support for Javascript, Sql, and C++ code. There is also preliminary
support for OCaml code so that the framework can be used on the code 
of pfff itself.

For each languages there are mainly 2 libraries, for instance
parsing_php.cma and analysis_php.cma, that you can
embed in your own application if you need to process PHP code. See the
demos/ directory for example of use of the pfff API. See also
docs/manual/Parsing_xxx.pdf and docs/manual/Analyzis_xxx.pdf for 
more documentation on how to use or extend pfff.

pfff is also made of few tools:
 - pfff, which allows to test the different parsers on a single file
 - pfff_db_light, which does some global analysis on a set of source files and
   store the data in a marshalled form in a file somewhere (e.g. /tmp/light_db)
 - pfff_db, which does some heavy language specific global analysis on a set
   of source files and store the data in a database somewhere 
   (e.g. /tmp/pfff_db)
 - pfff_visual, which is a gtk and cairo based source code 
   visualizer/navigator/searcher leveraging
   the information computed previously by pfff_db_light
 - sgrep, a syntactical grep
 - spatch, a syntacitcal patch
 - scheck, a bug finder
 - pfff_tags, an Emacs tag generator

For more information, look at the pfff wiki here:
 http://github.com/facebook/pfff/wiki/Main
as well as the docs/manual/ directory.

Usage for pfff:
-----------------

   $ ./pfff -parse_php demos/foo.php 

or

   $ ./pfff -dump_ast demos/foo.php 

You can also look at ./pfff --help

Usage for pfff_db_light:
-------------------

   $ ./pfff_db_light -lang ml -o /tmp/light_db.db ~/pfff

to analyze all the .ml and .mli files under ~/pfff and store metadata
information (the database) in /tmp/light_db.db

Usage for pfff_visual:
------------------------

  $ ./pfff_visual -with_info /tmp/light_db.db ~/pfff

This should launch a gtk-based GUI that allows you to visualize
source code and perform some code search.



Usage for pfff_db:
-------------------

   $ ./pfff_db -lang php -metapath /tmp/pfff_db ~/www/

to analyze all the .php and .phpt files under ~/www and store metadata
information (the database) in /tmp/pfff_db. It may takes some time,
for instance 20 minutes for a really big PHP codebase.

Once this is done you can use some of the flags of pfff_db to
do some analysis as in:

  $ ./pfff_db -deadcode_analysis /tmp/pfff_db
