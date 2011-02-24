all: unimap_to_ocaml

unimap_to_ocaml: unimap_to_ocaml.ml
	ocamlfind ocamlc -g -package str -linkpkg \
		-o unimap_to_ocaml \
		unimap_to_ocaml.ml

unihan_extract: unihan_extract.ml
	ocamlfind ocamlc -g -package str -linkpkg \
		-o unihan_extract \
		unihan_extract.ml

clean:
	rm -f *.cmi *.cmo *.cma *.cmx *.o *.a *.cmxa \
	unimap_to_ocaml unihan_extract

CLEAN: clean

distclean: clean
	rm -f *~ unimap_to_ocaml unihan_extract

