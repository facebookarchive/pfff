OCAMLMAKEFILE := ../../make/OCamlMakefile


RESULT = t
SOURCES = id_piqi.ml id_piqi_ext.ml example_piqi.ml example_piqi_ext.ml test.ml


PRE_TARGETS = id.proto.piqi example.proto.piqi id_piqi.ml id_piqi_ext.ml example_piqi.ml example_piqi_ext.ml


export OCAMLPATH = ../..
PACKS = piqirun.ext


PIQI ?= piqi
PIQIC = ../../piqic-ocaml/piqic-ocaml
PIQIC_FLAGS = --ext


all: native-code test #byte-code


test:
	rm -f *.piqi
	./$(RESULT)


%.proto.piqi: %.proto
	$(PIQI) of-proto $<


%_piqi.ml: %.proto.piqi
	$(PIQIC) $(PIQIC_FLAGS) $<


clean::
	rm -f *.tmp.ml *.piqi


include $(OCAMLMAKEFILE)
