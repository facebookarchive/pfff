
pfff
======

pfff is a set of tools and APIs to perform some static analysis, dynamic
analysis, code visualizations, code navigations, or style-preserving
source-to-source transformations such as refactorings on source code.
For now the effort is focused on PHP but there is preliminary support
for Javascript, C++, Erlang and other languages. There is also
preliminary support for OCaml code so that the framework can be used
on the code of pfff itself.

For each languages there are mainly 2 libraries, for instance
parsing_php.cma and analysis_php.cma, that you can
embed in your own application if you need to process PHP code. See the
demos/ directory for example of use of the pfff API. See also
docs/manual/Parsing_xxx.pdf and docs/manual/Analyzis_xxx.pdf for 
more documentation on how to use or extend pfff.

pfff is also made of few tools:
 - pfff, which allows to test the different parsers on a single file
 - pfff_db, which does some global analysis on a set of source files and
   store the data in a marshalled form in a file somewhere (e.g. ```/tmp/db.json```)
 - pfff_db_heavy, which does some heavy language specific global analysis 
   on a set of source files and store the data in a database somewhere
   (e.g. ```/tmp/pfff_db/```)
 - codemap, which is a gtk and cairo based source code 
   visualizer/navigator/searcher leveraging
   the information computed previously by pfff_db
 - sgrep, a syntactical grep
 - spatch, a syntactical patch
 - scheck, a bug finder
 - scheck_heavy, a bug finder leveraging the information computed previously
   by pfff_db_heavy
 - stags, an Emacs tag generator

For more information, look at the pfff wiki here:
 http://github.com/facebook/pfff/wiki/Main
as well as the ```docs/manual/``` directory.

Usage for pfff:
-----------------

   ```$ ./pfff -parse_php demos/foo.php``` 

or

   ```$ ./pfff -dump_php demos/foo.php``` 

You can also look at ```./pfff --help```

Usage for pfff_db:
-------------------

   ```$ ./pfff_db -lang ml -o /tmp/pfff.json ~/pfff```

to analyze all the ```.ml``` and ```.mli``` files under ```~/pfff``` and store metadata
information (the database) in ```/tmp/pfff.json```

Usage for codemap:
------------------------

  ```$ ./codemap -with_info /tmp/pfff.json ~/pfff```

This should launch a gtk-based GUI that allows you to visualize
source code and perform some code search.



Usage for pfff_db_heavy:
-----------------------------

For now only PHP is supported for the heavy analysis. To build
the heavy database do:

   ```$ ./pfff_db_heavy -metapath /tmp/pfff_db/ ~/www/```

to analyze all the ```.php``` and ```.phpt``` files under ~/www and store metadata
information (the database) in ```/tmp/pfff_db/```. It may takes some time,
for instance 20 minutes for a PHP codebase with many million lines of code.

Once this is done you can use some of the flags of pfff_db_heavy to
do some analysis as in:

  ```$ ./pfff_db_heavy -deadcode_analysis /tmp/pfff_db/```

More information
----------------------

Look at the pfff wiki [here](http://github.com/facebook/pfff/wiki/Main)
