By default, only the Javascript viewer is compiled.  To compile the
GTK viewer and the converter from dot file to JSON, you need Lablgtk2
and Cairo-OCaml.  Then, type:

    make -f Makefile-all

The dot format is partially supported at the moment: only some
basic styles are rendered correctly.
