all: unimap_to_ocaml

unimap_to_ocaml: unimap_to_ocaml.ml
	ocamlc \
		-o unimap_to_ocaml \
		str.cma \
		unimap_to_ocaml.ml

unihan_extract: unihan_extract.ml
	ocamlc \
		-o unihan_extract \
		str.cma \
		unihan_extract.ml

clean:
	rm -f *.cmi *.cmo *.cma *.cmx *.o *.a *.cmxa \
	unimap_to_ocaml unihan_extract

CLEAN: clean

distclean: clean
	rm -f *~ unimap_to_ocaml unihan_extract

