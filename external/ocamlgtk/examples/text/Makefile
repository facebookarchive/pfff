all: unicode_table unicode_viewer

unicode_table: unicode_table.ml
	ocamlc -I +lablgtk2 lablgtk.cma -o unicode_table unicode_table.ml
unicode_table.opt: unicode_table.ml
	ocamlopt -I +lablgtk2 lablgtk.cmxa -o unicode_table unicode_table.ml

unicode_viewer: unicode_viewer.ml
	ocamlc -I +lablgtk2 lablgtk.cma -o unicode_viewer unicode_viewer.ml
clean:
	rm *.cm* *.o unicode_viewer unicode_table

